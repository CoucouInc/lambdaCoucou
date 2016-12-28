module LambdaCoucou.Factoids where

import Control.Monad (forever)
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as BS
import LambdaCoucou.Types (Factoids)

import qualified Data.Aeson as JSON

factoidsPath :: FilePath
factoidsPath = "./resources/factoids.json"

readFactoids :: IO (Either String Factoids)
readFactoids = do
    raw <- BS.readFile factoidsPath
    return $ JSON.eitherDecode' raw

writeFactoids :: STM.TBQueue Factoids -> IO ()
writeFactoids queue = forever $ do
    newFactoids <- STM.atomically $ STM.readTBQueue queue
    let bs = JSON.encode newFactoids
    putStrLn "Saving json to file"
    BS.writeFile factoidsPath bs
