{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Url where

import Data.Text (Text)

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser

updateLastUrl :: Text -> IRC.StatefulIRC T.BotState ()
updateLastUrl raw =
    case Parser.parseUrl raw of
        Nothing -> return ()
        Just url -> do
            lastUrlT <- T._lastUrl <$> IRC.state
            liftIO $ print $ "updating last url to " <> url
            liftIO $ STM.atomically $ STM.writeTVar lastUrlT (Just url)
            return ()

urlInfo :: IRC.StatefulIRC T.BotState Text
urlInfo = do
    lastUrlT <- T._lastUrl <$> IRC.state
    lastUrl <- liftIO $ STM.readTVarIO lastUrlT
    return $ fromMaybe "No recent url to grab." lastUrl
