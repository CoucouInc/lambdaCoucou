{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
       (sendFactoid, adjustCounterFactoid, setFactoid, resetFactoid,
        deleteFactoid, augmentFactoid)
import LambdaCoucou.Social (incCoucou, sendCoucouCount)
import LambdaCoucou.Connection (sendPrivMsg)


handleCommand :: IRC.UnicodeEvent -> Text -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()

-- handleCommand :: Text -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand _ _ T.CoucouCmdNop = return ()
handleCommand _ target (T.CoucouCmdCancer search) = do
    liftIO . print $ "cancer maybe with search: " <> show search
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = desc <> ": " <> url
            sendPrivMsg target payload
handleCommand _ target (T.CoucouCmdFactoid name factoidType) = do
    liftIO . putStrLn $ "factoid command"
    case factoidType of
        T.GetFactoid -> sendFactoid target name
        T.IncFactoid -> adjustCounterFactoid succ target name
        T.DecFactoid -> adjustCounterFactoid pred target name
        T.SetFactoid val -> setFactoid target name val
        T.ResetFactoid val -> resetFactoid target name val
        T.DeleteFactoid -> deleteFactoid target name
        T.AugmentFactoid val -> augmentFactoid target name val
handleCommand ev target T.CoucouCmdIncCoucou = incCoucou (IRC._source ev) target
handleCommand ev target (T.CoucouCmdGetCoucou mbNick) = sendCoucouCount (IRC._source ev) target mbNick
