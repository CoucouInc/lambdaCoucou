module LambdaCoucou.Db where

import Control.Monad (forever)
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBQueue as Queue
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)

import qualified LambdaCoucou.Types as T

factoidsPath :: FilePath
factoidsPath = "./resources/factoids.json"

readFactoids :: IO (Either String T.Factoids)
readFactoids = JSON.eitherDecode' <$> BS.readFile factoidsPath

socialPath :: FilePath
socialPath = "./resources/socialdb.json"

readSocial :: IO (Either String T.SocialRecords)
readSocial = JSON.eitherDecode' <$> BS.readFile socialPath


updateDb :: T.WriterQueue -> IO ()
updateDb queue = forever $ do
    msg <- STM.atomically $ STM.readTBQueue queue
    case msg of
        Left facts -> writeFactoids facts
        Right social -> writeSocial social
    return ()

writeFactoids :: T.Factoids -> IO ()
writeFactoids newFactoids = do
    let bs = JSON.encode newFactoids
    atomicWriteFile factoidsPath bs

writeSocial :: T.SocialRecords -> IO ()
writeSocial newSocials = do
    let bs = JSON.encode newSocials
    atomicWriteFile socialPath bs

updateFactoids :: T.WriterQueue -> T.Factoids -> STM ()
updateFactoids queue factoids = Queue.writeTBQueue queue (Left factoids)

updateSocials :: T.WriterQueue -> T.SocialRecords -> STM ()
updateSocials queue socials = Queue.writeTBQueue queue (Right socials)
