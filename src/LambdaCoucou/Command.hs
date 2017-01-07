{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
       (getFactoid, adjustCounterFactoid, setFactoid, resetFactoid,
        deleteFactoid, augmentFactoid)
import LambdaCoucou.Social (incCoucou, getCoucouCount, getLastSeen)


handleCommand :: IRC.UnicodeEvent -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand _ T.CoucouCmdNop = return ()
handleCommand ev (T.CoucouCmdCancer search) = do
    liftIO . print $ "cancer with search: " <> show search
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = desc <> ": " <> url
            IRC.reply ev payload
handleCommand ev (T.CoucouCmdFactoid name factoidType) = do
    liftIO . putStrLn $ "factoid command"
    case factoidType of
        T.GetFactoid -> getFactoid name >>= sendReply ev
        T.IncFactoid -> adjustCounterFactoid succ ev name
        T.DecFactoid -> adjustCounterFactoid pred ev name
        T.SetFactoid val -> setFactoid ev name val
        T.ResetFactoid val -> resetFactoid ev name val
        T.DeleteFactoid -> deleteFactoid ev name
        T.AugmentFactoid val -> augmentFactoid ev name val
handleCommand ev T.CoucouCmdIncCoucou = incCoucou (IRC._source ev)
handleCommand ev (T.CoucouCmdGetCoucou mbNick) = getCoucouCount (IRC._source ev) mbNick >>= sendReply ev
handleCommand ev (T.CoucouCmdLastSeen nick) = getLastSeen nick >>= sendReply ev


sendReply :: IRC.UnicodeEvent -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendReply _ Nothing = return ()
sendReply ev (Just msg) = IRC.reply ev msg
