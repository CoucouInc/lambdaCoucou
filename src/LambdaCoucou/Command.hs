{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
       (sendFactoid, adjustCounterFactoid, setFactoid, resetFactoid,
        deleteFactoid, augmentFactoid)
import LambdaCoucou.Social (incCoucou, sendCoucouCount, sendLastSeen)


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
        T.GetFactoid -> sendFactoid ev name
        T.IncFactoid -> adjustCounterFactoid succ ev name
        T.DecFactoid -> adjustCounterFactoid pred ev name
        T.SetFactoid val -> setFactoid ev name val
        T.ResetFactoid val -> resetFactoid ev name val
        T.DeleteFactoid -> deleteFactoid ev name
        T.AugmentFactoid val -> augmentFactoid ev name val
handleCommand ev T.CoucouCmdIncCoucou = incCoucou (IRC._source ev) -- (IRC._source ev) target
handleCommand ev (T.CoucouCmdGetCoucou mbNick) = sendCoucouCount (IRC._source ev) ev mbNick
handleCommand ev (T.CoucouCmdLastSeen nick) = sendLastSeen ev nick
