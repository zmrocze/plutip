module Spec.Integration (test) where

import Control.Exception (ErrorCall, Exception (fromException))
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, Value, ciTxOutValue, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, ContractError (ConstraintResolutionContractError), submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Test.Plutip.Config (PlutipConfig (bpiForceBudget))
import Test.Plutip.Contract (ValueOrdering (VLt), assertExecution, initAda, initAndAssertAda, initAndAssertAdaWith, initLovelace, withContract, withContractAs)
import Test.Plutip.Internal.Types (
  FailureReason (CaughtException),
  isException,
 )
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Predicate (errorSatisfies, failReasonSatisfies, shouldFail, shouldSucceed, shouldThrow, shouldYield, stateIs, stateSatisfies, yieldSatisfies)
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (TestTree)
import Text.Printf (printf)

-- FIXME: something prints node configs polluting test outputs even with maximum log severity
-- upd: (https://github.com/input-output-hk/cardano-node/blob/4ad6cddd40517c2eb8c3df144a6fa6737952aa92/cardano-node/src/Cardano/Node/Run.hs#L117)

test :: TestTree
test =
  withConfiguredCluster
    (def {bpiForceBudget = Just (100, 100)})
    "Basic integration: launch, add wallet, tx from wallet to wallet"
    [ -- Basic Succeed or Failed tests
      assertExecution
        "Contract 1"
        (initAda 100)
        (withContract $ const getUtxos)
        [ shouldSucceed
        , Predicate.not shouldFail
        ]
    , assertExecution
        "Contract 2"
        (initAda 100)
        (withContract $ const getUtxosThrowsErr)
        [ shouldFail
        , Predicate.not shouldSucceed
        ]
    , assertExecution
        "Contract 3"
        (initAda 100)
        (withContract $ const getUtxosThrowsEx)
        [ shouldFail
        , Predicate.not shouldSucceed
        ]
    , assertExecution
        "Pay negative amount"
        (initAda 100)
        (withContract $ \[pkh1] -> payTo pkh1 (-10_000_000))
        [shouldFail]
    , -- Tests with wallet's Value assertions
      assertExecution
        "Pay from wallet to wallet"
        (initAda 100 <> initAndAssertAda 100 110)
        (withContract $ \[pkh1] -> payTo pkh1 10_000_000)
        [shouldSucceed]
    , assertExecution
        "Two contracts one after another"
        ( initAndAssertAdaWith 100 VLt 100 -- own wallet (index 0 in wallets list)
            <> initAndAssertAdaWith 100 VLt 100 -- wallet with index 1 in wallets list
        )
        ( do
            void $ -- run something prior to the contract which result will be checked
              withContract $
                \[pkh1] -> payTo pkh1 10_000_000
            withContractAs 1 $ -- run contract which result will be checked
              \[pkh1] -> payTo pkh1 10_000_000
        )
        [shouldSucceed]
    , -- Tests with assertions on Contract return value
      assertExecution
        "Initiate wallet and get UTxOs"
        (initAda 100)
        (withContract $ const getUtxos)
        [ yieldSatisfies "Returns single UTxO" ((== 1) . Map.size)
        ]
    , let initFunds = 10_000_000
       in assertExecution
            "Should yield own initial Ada"
            (initLovelace $ toEnum initFunds)
            (withContract $ const ownValue)
            [ shouldYield (lovelaceValueOf $ toEnum initFunds)
            ]
    , -- Tests with assertions on state
      let initFunds = 10_000_000
       in assertExecution
            "Puts own UTxOs Value to state"
            (initLovelace $ toEnum initFunds)
            (withContract $ const ownValueToState)
            [ stateIs [lovelaceValueOf $ toEnum initFunds]
            , Predicate.not $ stateSatisfies "length > 1" ((> 1) . length)
            ]
    , -- Tests with assertions on failure
      let expectedErr = ConstraintResolutionContractError OwnPubKeyMissing
          isResolutionError = \case
            ConstraintResolutionContractError _ -> True
            _ -> False
       in assertExecution
            ("Contract which throws `" <> show expectedErr <> "`")
            (initAda 100)
            (withContract $ const getUtxosThrowsErr)
            [ shouldThrow expectedErr
            , errorSatisfies "Throws resolution error" isResolutionError
            , Predicate.not $ failReasonSatisfies "Throws exception" isException
            ]
    , let checkException = \case
            CaughtException e -> isJust @ErrorCall (fromException e)
            _ -> False
       in assertExecution
            "Contract which throws exception"
            (initAda 100)
            (withContract $ const getUtxosThrowsEx)
            [ shouldFail
            , Predicate.not shouldSucceed
            , failReasonSatisfies "Throws ErrorCall" checkException
            ]
    ]

getUtxos :: Contract [Value] EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxos = do
  pkh <- Contract.ownPaymentPubKeyHash
  Contract.logInfo @String $ printf "Own PKH: %s" (show pkh)
  utxosAt $ pubKeyHashAddress pkh Nothing

getUtxosThrowsErr :: Contract () EmptySchema ContractError (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsErr =
  Contract.throwError $ ConstraintResolutionContractError OwnPubKeyMissing

getUtxosThrowsEx :: Contract () EmptySchema Text (Map TxOutRef ChainIndexTxOut)
getUtxosThrowsEx = error "This Exception was thrown intentionally in Contract.\n"

payTo :: PaymentPubKeyHash -> Integer -> Contract () EmptySchema Text CardanoTx
payTo toPkh amt = do
  submitTx (Constraints.mustPayToPubKey toPkh (Ada.lovelaceValueOf amt))

ownValue :: Contract [Value] EmptySchema Text Value
ownValue = foldMap (^. ciTxOutValue) <$> getUtxos

-- this Contract fails, but state should change in expected way
ownValueToState :: Contract [Value] EmptySchema Text ()
ownValueToState = do
  ownValue >>= Contract.tell . (: [])
  void $ Contract.throwError "Intentional fail"
  ownValue >>= Contract.tell . (: []) -- should not be in state
