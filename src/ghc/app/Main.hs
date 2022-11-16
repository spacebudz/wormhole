module Main where

import Onchain (mintSerialized, referenceSerialized, lockSerialized)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import GHC.Generics (Generic)
import Prelude

data Scripts = Scripts {mint :: String, reference :: String, lock :: String} deriving (Show, Generic, ToJSON)

scripts :: Scripts
scripts = Scripts {mint = mintSerialized, reference = referenceSerialized, lock = lockSerialized}

main :: IO ()
main = do
  I.writeFile "scripts.json" (encodeToLazyText scripts)
  putStrLn "Scripts compiled"