module Main (main) where
import Data.String (fromString)
import Plutus.V1.Ledger.Bytes (LedgerBytes(getLedgerBytes), fromHex)
import Plutus.V1.Ledger.Api (fromBuiltin, BuiltinByteString, Script, LedgerBytes (LedgerBytes), toBuiltin, Data (..), toData)
import Data.ByteString (ByteString)
import Prelude
import Codec.Serialise (deserialise, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Ledger (Script(unScript), applyArguments)
import UntypedPlutusCore.Core (progTerm)
import Control.Lens ((^.))
import Data.Word (Word8)
import qualified Data.ByteString as B
showHex :: ByteString -> String
showHex = show . LedgerBytes . toBuiltin
-- Awlayssucceeds v1
script0a = toScript "4d01000033222220051200120011"
script1a = toScript "581a0100003333222220051200120014c103d87980004c0101050001"
script2a = toScript "4d0100330034c104d8799fff004c0101050001"

script0a_haskell_unit = toScript "55010000333222220051200120014c0103d879800001"
script0a_wasm_unit = toScript    "56010000333222220051200120014c0104d8799fff0001"

script0a_applied45 = toScript "58180100003333222220051200120014c10104004c0101050001" -- ctl server

script2aa = toScript "534d0100330034c104d8799fff004c0101050001"

scriptalways_succeed_4_5 = toScript "0100003333222220051200120014c10104004c0101050001" -- bad, js
applied_4_5_rust_bytes = B.pack [88, 24, 1, 0, 0, 51, 51, 34, 34, 32, 5, 18, 0, 18, 0, 20, 193, 1, 4, 0, 76, 1, 1, 5, 0, 1]
-- scriptalways_succeed_4_5_rust = deserialise $ fromStrict applied_4_5_rust_bytes


res_words_2a :: [Word8]
res_words_2a = [83, 77, 1, 0, 51, 0, 52, 193, 4, 216, 121, 159, 255, 0, 76, 1, 1, 5, 0, 1]
res_bytes_2a = LedgerBytes $ toBuiltin $ B.pack res_words_2a

xxxx = "58180100003333222220051200120014c10104004c0101050001"

toScript :: String -> Script
toScript = deserialise . fromStrict . fromBuiltin . getLedgerBytes . fromString
script0 = toScript "540100002225333573466e1cdd6801a40a82930b01"
script1 = toScript "5821010000332225333573466e1cdd6801a40a82930b260103d87980004c0101050001"
script2 = toScript "540100330024c104d8799fff004c0101050001"

-- scripts = [script0, script1, script2, script0a, script1a, script2a]
scripts = [script0a, script0a_haskell_unit, script0a_wasm_unit]

getLambda scr = unScript scr ^. progTerm

unt = Constr 0 []
pltmap = Map [(I 1, unt)]
pltlist = List [I 0, I 1, I 2]

pltData = [unt, pltmap, pltlist]

main :: IO ()
main = do
  print $ "unit serialized" <> showHex (toStrict $ serialise $ toData ())
  print $ "should be: " <> show (applyArguments script0a [toData ()])
  mapM_ (print . unScript) scripts

  print $ script0a_haskell_unit == script0a_wasm_unit
-- main :: IO ()
-- main = print $ 
--   map ( showHex . toStrict . serialise . List . pure ) pltData