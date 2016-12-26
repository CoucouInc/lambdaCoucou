{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Network.IRC.Client as IRC
import qualified Network.HTTP.Req

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)


handleCommand :: Text -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand _ T.CoucouCmdNop = return ()

handleCommand target (T.CoucouCmdCancer search) = do
    liftIO . print $ "cancer maybe with search: " <> show search
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = desc <> ": " <> url
            let response = IRC.Privmsg target (Right payload)
            IRC.send response
