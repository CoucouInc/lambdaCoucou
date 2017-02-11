{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCoucou.Url where

import Data.Text (Text, unpack)

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, maybe)
import Control.Exception
import qualified Data.Text.Encoding as Enc
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Types.Status (statusCode)
import qualified Text.HTML.TagSoup as HTML


updateLastUrl :: IRC.Source Text -> Text -> IRC.StatefulIRC T.BotState ()
updateLastUrl (IRC.Channel _ _) raw =
    case Parser.parseUrl raw of
        Nothing -> return ()
        Just url -> do
            lastUrlT <- T._lastUrl <$> IRC.state
            liftIO $ print $ "updating last url to " <> url
            liftIO $ STM.atomically $ STM.writeTVar lastUrlT (Just url)
            return ()
updateLastUrl _ _ = return ()

urlInfo :: IRC.StatefulIRC T.BotState Text
urlInfo = do
    lastUrlT <- T._lastUrl <$> IRC.state
    lastUrl <- liftIO $ STM.readTVarIO lastUrlT
    case lastUrl of
        Nothing -> return $ fromMaybe "No recent url to grab." lastUrl
        Just url -> do
            mbTitle <- liftIO $ grabTitle url
            let errMsg = "Cannot find title for " <> url
            let resp = maybe errMsg (\t -> t <> " [" <> url <> "]") mbTitle
            return resp


grabTitle :: Text -> IO (Maybe Text)
grabTitle url = handle (\(_ :: SomeException) -> return Nothing) $ do
    manager <- HTTP.newManager HTTPS.tlsManagerSettings
    request <- HTTP.parseRequest (unpack url)
    response <- HTTP.httpLbs request manager
    putStrLn $ "The status code was: " ++ show (statusCode $ HTTP.responseStatus response)
    let decodedBody = Enc.decodeUtf8 $ BS.concat $ LBS.toChunks $ HTTP.responseBody response
    let tags = HTML.parseTags decodedBody
    let mbTitle = take 1 $ tail $ dropWhile (HTML.~/= HTML.TagOpen ("title" :: Text) []) tags
    case mbTitle of
        [] -> return Nothing
        (title:_) -> do
            print $ "raw title: " <> HTML.fromTagText title
            return $ Just (HTML.fromTagText title)
