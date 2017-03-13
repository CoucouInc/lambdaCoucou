{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCoucou.Url (updateLastUrl, urlInfo) where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, maybe)
import Control.Exception
import qualified Data.List as List
import qualified Data.Text.Encoding as Enc
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (CI)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
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
grabTitle url =
    handle (\(_ :: SomeException) -> return Nothing) $
    do manager <- HTTP.newManager HTTPS.tlsManagerSettings
       request <- HTTP.parseRequest (Text.unpack url)
       response <- HTTP.httpLbs request manager
       -- putStrLn $ "The status code was: " ++ show (statusCode $ HTTP.responseStatus response)
       -- print $ "headers: " <> show (HTTP.responseHeaders response)
       let body = HTTP.responseBody response
       case contentType (HTTP.responseHeaders response) of
           Nothing -> return $ parseTitle body -- still try
           Just t ->
               let r
                       | Text.isInfixOf "text" (Text.toLower t) = parseTitle body
                       | Text.isInfixOf "html" (Text.toLower t) = parseTitle body
                       | Text.isPrefixOf "image" (Text.toLower t) =
                           Just "That's an image, can't display that"
                       | Text.isPrefixOf "video" (Text.toLower t) =
                           Just "That's a video, can't display that"
                       | otherwise = Nothing
               in return r

contentType :: ResponseHeaders -> Maybe Text
contentType headers =
    let mbHeader = List.find ((== ("content-type" :: CI BS.ByteString)) . fst) headers
        hush (Left _) = Nothing
        hush (Right a) = Just a
    in case mbHeader of
           Nothing -> Nothing
           Just (_, val) -> hush (Enc.decodeUtf8' val)

parseTitle :: LBS.ByteString -> Maybe Text
parseTitle body =
    let decodedBody = Enc.decodeUtf8 $ BS.concat $ LBS.toChunks body
        tags = HTML.parseTags decodedBody
        mbTitle = tail $ dropWhile (HTML.~/= HTML.TagOpen ("title" :: Text) []) tags
    in case mbTitle of
           [] -> Nothing
           (title:_) -> Just (HTML.fromTagText title)
