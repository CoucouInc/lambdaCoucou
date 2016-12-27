module LambdaCoucou.Factoids where

import qualified Data.ByteString.Lazy as BS
import LambdaCoucou.Types (Factoids)

import qualified Data.Aeson as JSON

readFactoids :: IO (Either String Factoids)
readFactoids = do
    raw <- BS.readFile "./resources/factoids.json"
    return $ JSON.eitherDecode' raw
