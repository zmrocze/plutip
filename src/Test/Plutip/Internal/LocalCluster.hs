module Test.Plutip.Internal.LocalCluster (
  startCluster,
  stopCluster,
  withPlutusInterface,
  -- withPlutusInterface',
) where

import Cardano.Api qualified as CAPI

import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Startup (installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding)
import Cardano.Wallet.Logging (stdoutTextTracer, trMessageText)
import Cardano.Wallet.Shelley.Launch (withSystemTempDir)

import Cardano.BM.Data.Severity qualified as Severity
import Cardano.BM.Data.Tracer (HasPrivacyAnnotation, HasSeverityAnnotation (getSeverityAnnotation))
import Cardano.CLI (LogOutput (LogToFile, LogToStdStreams), withLoggingNamed)
import Cardano.Wallet.Shelley.Launch.Cluster (ClusterLog, localClusterConfigFromEnv, testMinSeverityFromEnv, walletMinSeverityFromEnv, withCluster)
import Control.Concurrent.Async (async)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Retry (constantDelay, limitRetries, recoverAll)
import Control.Tracer (Tracer, traceWith)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text (Text, pack)
import Data.Text.Class (ToText (toText))
import Plutus.ChainIndex.App qualified as ChainIndex
import Plutus.ChainIndex.Config (ChainIndexConfig (cicNetworkId, cicPort), cicDbPath, cicSocketPath)
import Plutus.ChainIndex.Config qualified as ChainIndex
import Plutus.ChainIndex.Logging (defaultConfig)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import System.Directory (copyFile, findExecutable, getDirectoryContents)
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath (takeExtension, (</>))
import Test.Plutip.Internal.BotPlutusInterface.Setup qualified as BotSetup
import Test.Plutip.Internal.Types (
  ClusterEnv (ClusterEnv, bpiForceBudget, chainIndexUrl, networkId, runningNode, supportDir, tracer),
  RunningNode (RunningNode),
 )
import Test.Plutip.Tools.CardanoApi qualified as Tools
import Test.Plutip.Tools.ChainIndex qualified as Tools
import UnliftIO.Concurrent (forkFinally)
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, retrySTM, writeTVar)

import Control.Exception (SomeException, try)
import Data.Foldable (for_)
import Data.Map qualified as Map
import GHC.Stack.Types (HasCallStack)
import Ledger (TxOutRef (TxOutRef), toTxOut, ciTxOutDatum, datumHash, TxOut (txOutDatumHash))
import Ledger.Tx.CardanoAPI (fromCardanoTxId, toCardanoTxOut)
import Paths_plutip (getDataFileName)
import Test.Plutip.Config (PlutipConfig (chainIndexPort, clusterDataDir, relayNodeLogs))
import Test.Plutip.Config qualified as Config
import Text.Printf (printf)
import UnliftIO.Exception (catchIO)
import Control.Monad (forM_)
import Control.Monad ((>=>))
import Control.Lens

-- | Starting a cluster with a setup action
-- We're heavily depending on cardano-wallet local cluster tooling, however they don't allow the
-- start and stop actions to be two separate processes, which is needed for tasty integration.
-- Instead of rewriting and maintaining these, I introduced a semaphore mechanism to keep the
-- cluster alive until the ClusterClosing action is called.
startCluster ::
  forall (a :: Type).
  PlutipConfig ->
  ReaderT ClusterEnv IO a ->
  IO (TVar (ClusterStatus a), a)
startCluster conf onClusterStart = do
  status <- newTVarIO ClusterStarting
  void $
    forkFinally
      ( withPlutusInterface conf $ \clusterEnv -> do
          res <- runReaderT onClusterStart clusterEnv
          atomically $ writeTVar status (ClusterStarted res)
          atomically $ readTVar status >>= \case ClusterClosing -> pure (); _ -> retrySTM
      )
      (either (error . show) (const (atomically (writeTVar status ClusterClosed))))

  setupRes <- atomically $ readTVar status >>= \case ClusterStarted v -> pure v; _ -> retrySTM
  pure (status, setupRes)

--- | Send a shutdown signal to the cluster and wait for it
stopCluster :: TVar (ClusterStatus a) -> IO ()
stopCluster status = do
  atomically $ writeTVar status ClusterClosing
  atomically $ readTVar status >>= \case ClusterClosed -> pure (); _ -> retrySTM

{- Examples:
   `plutus-apps` local cluster: https://github.com/input-output-hk/plutus-apps/blob/75a581c6eb98d36192ce3d3f86ea60a04bc4a52a/plutus-pab/src/Plutus/PAB/LocalCluster/Run.hs
   `cardano-wallet` local cluster: https://github.com/input-output-hk/cardano-wallet/blob/99b13e50f092ffca803fd38b9e435c24dae05c91/lib/shelley/exe/local-cluster.hs
-}
withPlutusInterface :: forall (a :: Type). PlutipConfig -> (ClusterEnv -> IO a) -> IO a
withPlutusInterface conf action = do
  -- current setup requires `cardano-node` and `cardano-cli` as external processes
  checkProcessesAvailable ["cardano-node", "cardano-cli"]

  withLocalClusterSetup conf $ \dir clusterLogs _walletLogs -> do
    result <- withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
      let tr' = contramap MsgCluster $ trMessageText trCluster
      clusterCfg <- localClusterConfigFromEnv
      withCluster
        tr'
        dir
        clusterCfg
        (const $ pure ()) -- faucet setup was here in `cardano-wallet` version
        (\rn -> runActionWthSetup rn dir trCluster action)
    handleLogs dir conf
    return result
  where
    runActionWthSetup rn dir trCluster userActon = do
      let tracer' = trMessageText trCluster
      waitForRelayNode tracer' rn
      ciPort <- launchChainIndex conf rn dir
      traceWith tracer' (ChaiIndexStartedAt ciPort)
      let cEnv =
            ClusterEnv
              { runningNode = rn
              , chainIndexUrl = BaseUrl Http "localhost" ciPort mempty
              , networkId = CAPI.Mainnet
              , supportDir = dir
              , tracer = trCluster
              , bpiForceBudget = Config.bpiForceBudget conf
              }

      BotSetup.runSetup cEnv -- run preparations to use `bot-plutus-interface`
      res <- userActon cEnv -- executing user action on cluster
      try @SomeException (debugTxCheck cEnv) >>= print
      return res

debugTxCheck :: ClusterEnv -> IO ()
debugTxCheck cEnv = do
  let dir = BotSetup.txsDir cEnv
  signedTxFiles <- findSignedTxs dir
  putStrLn $ BotSetup.txsDir cEnv
  putStrLn $ "Signed txs" <> show signedTxFiles

  let absSignedTxFiles = map (dir </>) signedTxFiles
  forM_ absSignedTxFiles (readFile >=> print)
  forM_ absSignedTxFiles doThings
  where
    doThings tx =
      deserialise tx
        >>= getExUnits cEnv
        >>= putStrLn . ("ExUnits: " ++) . show

    findSignedTxs path =
      filter ((".signed" ==) . takeExtension)
        <$> getDirectoryContents path

deserialise :: FilePath -> IO (CAPI.Tx CAPI.AlonzoEra)
deserialise txFile = do
  env <- either (error . show) id <$> CAPI.readTextEnvelopeFromFile txFile
  return $
    either
      (error . show)
      id
      (CAPI.deserialiseFromTextEnvelope CAPI.AsAlonzoTx env)

getExUnits cEnv tx = do
  -- forever (threadDelay 3000000000000000)
  let txBody = CAPI.getTxBody tx
  sysStart <- getOrFail <$> Tools.systemStart cEnv
  eraHist <- getOrFail <$> Tools.eraHistory cEnv
  pparams <- getOrFail <$> Tools.protocolParams cEnv
  utxo <- getUtxo txBody
  return $
        CAPI.evaluateTransactionExecutionUnits
          CAPI.AlonzoEraInCardanoMode
          sysStart
          eraHist
          pparams
          utxo
          txBody
  where
    getUtxo txBody = do
      let (CAPI.TxBody txbc) = txBody
          (capiIn : _) = fst <$> CAPI.txIns txbc
          CAPI.TxIn txid (CAPI.TxIx ix) = capiIn

          oref :: TxOutRef = TxOutRef (fromCardanoTxId txid) (toInteger ix)

      ciTxOut <- getOrFail <$> Tools.getTxOut (chainIndexUrl cEnv) oref
      putStrLn $ "cix out: " <> show ciTxOut

      let plutusOut = toTxOut ciTxOut
          datsMap = mkDatums [ciTxOut]
          capiOut =
            CAPI.toCtxUTxOTxOut -- here datum will be converted to hash
              . getOrFail
              $ toCardanoTxOut Tools.netId datsMap plutusOut
      putStrLn $ "SEARCH: " ++ show (txOutDatumHash plutusOut >>= flip Map.lookup datsMap)
      putStrLn $ "PLUTUS out: " ++ show plutusOut
      putStrLn $ "INDEX out: " ++ show ciTxOut
      putStrLn $ "DATS map: " ++ show datsMap
      putStrLn $ "CAPI out: " ++ show capiOut
      return $
        CAPI.UTxO $ Map.fromList [(capiIn, capiOut)]

    mkDatums = Map.fromList 
                . map (\d -> (datumHash d, d)) 
                . mapMaybe (^? ciTxOutDatum . _Right)

    getOrFail :: Show e => Either e a -> a
    getOrFail = either (error . show) id



-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup ::
  forall (a :: Type).
  PlutipConfig ->
  (FilePath -> [LogOutput] -> [LogOutput] -> IO a) ->
  IO a
withLocalClusterSetup conf action = do
  -- Setting required environment variables
  setEnv "NO_POOLS" "1"
  setClusterDataDir

  -- Handle SIGTERM properly
  installSignalHandlers (putStrLn "Terminated")

  -- Ensure key files have correct permissions for cardano-cli
  setDefaultFilePermissions

  -- Set UTF-8, regardless of user locale
  withUtf8Encoding $
    -- This temporary directory will contain logs, and all other data
    -- produced by the local test cluster.
    withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
      let logOutputs name minSev =
            [ LogToFile (dir </> name) (min minSev Severity.Info)
            , LogToStdStreams minSev
            ]

      clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
      walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

      action dir clusterLogs walletLogs
  where
    setClusterDataDir = do
      defaultClusterDataDir <- getDataFileName "cluster-data"
      setEnv "SHELLEY_TEST_DATA" $
        fromMaybe defaultClusterDataDir (clusterDataDir conf)

checkProcessesAvailable :: [String] -> IO ()
checkProcessesAvailable requiredProcesses = do
  results <- mapM findExecutable requiredProcesses
  unless (isJust `all` results) $
    die $
      "This processes should be available in the environment:\n " <> show requiredProcesses
        <> "\n but only these were found:\n "
        <> show (catMaybes results)

waitForRelayNode :: Tracer IO TestsLog -> RunningNode -> IO ()
waitForRelayNode trCluster rn = do
  liftIO $ recoverAll policy (const getTip)
  where
    policy = constantDelay 500000 <> limitRetries 5
    getTip = trace >> void (Tools.queryTip rn)
    trace = traceWith trCluster WaitingRelayNode

-- | Launch the chain index in a separate thread.
launchChainIndex :: PlutipConfig -> RunningNode -> FilePath -> IO Int
launchChainIndex conf (RunningNode sp _block0 (_gp, _vData)) dir = do
  config <- defaultConfig
  let dbPath = dir </> "chain-index.db"
      chainIndexConfig =
        ChainIndex.defaultConfig
          { cicSocketPath = nodeSocketFile sp
          , cicDbPath = dbPath
          , cicNetworkId = CAPI.Mainnet
          , cicPort =
              maybe
                (cicPort ChainIndex.defaultConfig)
                fromEnum
                (chainIndexPort conf)
          }
  void . async $ void $ ChainIndex.runMain config chainIndexConfig
  return $ cicPort chainIndexConfig

handleLogs :: HasCallStack => FilePath -> PlutipConfig -> IO ()
handleLogs clusterDir conf =
  copyRelayLog `catchIO` (error . printf "Failed to save relay node log: %s" . show)
  where
    copyRelayLog = for_ (relayNodeLogs conf) $ \toFile ->
      copyFile
        {- We're heavily depending on cardano-wallet local cluster tooling atm.
          Path partially hardcoded in Cardano.Wallet.Shelley.Launch.Cluster by
         `withRelayNode` ("node" subdir) and `genConfig` (file name)
        -}
        (clusterDir </> "node" </> "cardano-node.log")
        toFile

data ClusterStatus (a :: Type)
  = ClusterStarting
  | ClusterStarted a
  | ClusterClosing
  | ClusterClosed

-- Logging

data TestsLog
  = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
  | MsgSettingUpFaucet
  | MsgCluster ClusterLog
  | WaitingRelayNode
  | ChaiIndexStartedAt Int
  deriving stock (Show)

instance ToText TestsLog where
  toText = \case
    MsgBaseUrl walletUrl ekgUrl prometheusUrl ->
      mconcat
        [ "Wallet url: "
        , walletUrl
        , ", EKG url: "
        , ekgUrl
        , ", Prometheus url:"
        , prometheusUrl
        ]
    MsgSettingUpFaucet -> "Setting up faucet..."
    MsgCluster msg -> toText msg
    WaitingRelayNode -> "Waiting for relay node up and running"
    ChaiIndexStartedAt ciPort -> "Chain-index started at port " <> pack (show ciPort)

instance HasPrivacyAnnotation TestsLog

instance HasSeverityAnnotation TestsLog where
  getSeverityAnnotation = \case
    MsgSettingUpFaucet -> Severity.Notice
    MsgBaseUrl {} -> Severity.Notice
    MsgCluster msg -> getSeverityAnnotation msg
    WaitingRelayNode -> Severity.Notice
    ChaiIndexStartedAt {} -> Severity.Notice
