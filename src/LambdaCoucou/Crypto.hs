{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCoucou.Crypto (fetchCrypto) where

import Data.Scientific
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Network.IRC.Client as IRC
import Network.HTTP.Req
import Control.Monad.IO.Class
import qualified Data.ByteString as B

import Control.Exception
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types as HT

import Control.Lens
import Data.Aeson.Lens

import qualified LambdaCoucou.Types as T

fetchCrypto :: Text -> IRC.StatefulIRC T.BotState (Maybe Text)
fetchCrypto symbol = do
    let target = "eur"
    r <- liftIO $ catch (fetch symbol target) (handleHttp symbol)
    pure $ Just r

fetch :: Text -> Text -> IO Text
fetch symbol target = do
    resp <- liftIO (getCrypto symbol target)
    case parseResponse (responseBody resp) of
        Nothing -> do
            putStrLn "Cannot parse response from cryto api"
            pure "Something wrong happened. Error has been logged"
        Just p ->
            pure
                $  "1 "
                <> symbol
                <> " is worth (?) "
                <> pack (show p)
                <> " "
                <> target


getCrypto :: (MonadIO m) => Text -> Text -> m BsResponse
getCrypto symbol target = do
    let pair = symbol <> target
    liftIO $ T.runHttp $ req
        GET
        (https "api.cryptowat.ch" /: "markets" /: "bitfinex" /: pair /: "price")
        NoReqBody
        bsResponse
        mempty

parseResponse :: B.ByteString -> Maybe Scientific
parseResponse raw = raw ^? key "result" . key "price" . _Number

handleHttp :: Text -> HttpException -> IO Text
handleHttp symbol (VanillaHttpException (H.HttpExceptionRequest _req (H.StatusCodeException resp _msg)))
    | s == HT.status400
    = pure $ "Currency not found: " <> symbol
    | HT.statusCode s == 429
    = pure "Rate limit exceeded"
    | otherwise
    = error "wip"
    where s = H.responseStatus resp

handleHttp symbol e = do
    liftIO $ putStrLn $ "Error for " <> unpack symbol <> " : " <> show e
    pure "Something wrong happened. Error has been logged"
