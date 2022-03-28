module Test.Plutip.Tools.ChainIndex (utxosAtPkh, getTxOut) where

import Cardano.Pool.Metadata (newManager)
import Data.Default (Default (def))
import Ledger (ChainIndexTxOut, PubKeyHash, TxOutRef)
import Network.HTTP.Client (defaultManagerSettings)
import Plutus.ChainIndex.Api (UtxoAtAddressRequest (UtxoAtAddressRequest), UtxosResponse)
import Plutus.ChainIndex.Client qualified as ChainIndexClient
import Plutus.V1.Ledger.Credential (Credential (PubKeyCredential))
import Servant.Client (
  BaseUrl,
  ClientError,
  mkClientEnv,
  runClientM,
 )

utxosAtPkh :: BaseUrl -> PubKeyHash -> IO (Either ClientError UtxosResponse)
utxosAtPkh url pkh = do
  manager <- newManager defaultManagerSettings
  runClientM client $ mkClientEnv manager url
  where
    client = ChainIndexClient.getUtxoSetAtAddress req
    req = UtxoAtAddressRequest (Just def) (PubKeyCredential pkh)

getTxOut :: BaseUrl -> TxOutRef -> IO (Either ClientError ChainIndexTxOut)
getTxOut url oref = do
  manager <- newManager defaultManagerSettings
  runClientM client $ mkClientEnv manager url
  where
    client = ChainIndexClient.getTxOut oref
