{-# LANGUAGE ViewPatterns #-}

-- | Helpers based on Cardano.Api (do not use `cardano-cli` executable)
module Test.Plutip.Tools.CardanoApi (
  currentBlock,
  utxosAtAddress,
  queryProtocolParams,
  queryTip,
) where

import Cardano.Api qualified as C
import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Cardano.Launcher.Node (nodeSocketFile)
import Cardano.Slotting.Slot (WithOrigin)
import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
import Control.Exception (Exception)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Test.Plutip.Internal.Types (ClusterEnv (runningNode))

data CardanoApiError
  = SomeError String
  deriving stock (Eq, Show, Generic)

instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
currentBlock :: ClusterEnv -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
currentBlock (runningNode -> rn) = do
  let query = C.QueryChainBlockNo
      info = connectionInfo rn
  C.queryNodeLocalState info Nothing query

utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.AlonzoEra))
utxosAtAddress (runningNode -> rn) addr = do
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query =
      shellyBasedAlonzoQuery
        (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr))

queryProtocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
queryProtocolParams (runningNode -> rn) =
  flattenQueryResult <$> C.queryNodeLocalState info Nothing query
  where
    info = connectionInfo rn
    query = shellyBasedAlonzoQuery C.QueryProtocolParameters

connectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfo (RunningNode socket _ _) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600))
    C.Mainnet
    (nodeSocketFile socket)

queryTip :: RunningNode -> IO C.ChainTip
queryTip = C.getLocalChainTip . connectionInfo

shellyBasedAlonzoQuery ::
  C.QueryInShelleyBasedEra C.AlonzoEra result ->
  C.QueryInMode C.CardanoMode (Either EraMismatch result)
shellyBasedAlonzoQuery =
  C.QueryInEra C.AlonzoEraInCardanoMode
    . C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo

flattenQueryResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either CardanoApiError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ SomeError (show err)
