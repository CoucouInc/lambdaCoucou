{-# LANGUAGE OverloadedStrings #-}

-- small tool to convert json file with unix timestamp to ISO8601
module LambdaCoucou.Convert where

import LambdaCoucou.Types as T
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS

convert :: IO ()
convert = do
    raw <- BS.readFile "./resources/socialdb.json"
    let parsed = JSON.eitherDecode' raw :: Either String T.SocialRecords
    case parsed of
        Left err -> print err
        Right p -> BS.writeFile "./foo.json" (JSON.encode p)
