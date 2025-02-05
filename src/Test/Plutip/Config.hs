module Test.Plutip.Config (
  PlutipConfig (..),
) where

import Data.Default (Default, def)
import GHC.Generics (Generic)
import GHC.Natural (Natural)

-- | Plutip configurable options
--
-- @since 0.2
data PlutipConfig = PlutipConfig
  { -- | in case of `Nothing` cluster data from project `data-files` is used
    clusterDataDir :: Maybe FilePath
  , -- | in case of `Just path` relay node log will be saved to specified file
    relayNodeLogs :: Maybe FilePath
  , -- | in case of `Nothing` port from `Plutus.ChainIndex.Config.defaultConfig` is used
    chainIndexPort :: Maybe Natural
  , -- | set the budget estimation to a constant
    bpiForceBudget :: Maybe (Integer, Integer)
  }
  deriving stock (Generic)

instance Default PlutipConfig where
  def = PlutipConfig Nothing Nothing Nothing Nothing
