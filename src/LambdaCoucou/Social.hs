{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Social where

import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified Data.HashMap.Strict as Map

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Connection (sendPrivMsg)
import LambdaCoucou.Db (updateSocials)

incCoucou :: IRC.Source Text -> Text -> IRC.StatefulIRC T.BotState ()
incCoucou (IRC.Channel _chanName user) _ = do
    liftIO . print $ "incCoucou count for " <> user <> " " <> pack (show T.CoucouCmdIncCoucou)
    state <- IRC.state
    let socialT = T._socialDb state
    liftIO $
        STM.atomically $
        do social <- STM.readTVar socialT
           let defaultUsr =
                   T.SocialRecord
                   { T._coucous = 0
                   }
           let socialUsr = Map.lookupDefault defaultUsr user social
           -- TODO maybe bring some lens here ?
           let socialUsr' =
                   socialUsr
                   { T._coucous = succ (T._coucous socialUsr)
                   }
           let social' = Map.insert user socialUsr' social
           STM.writeTVar socialT social'
           updateSocials (T._writerQueue state) social'

-- TODO decrease coucou count if sent from private message
incCoucou (IRC.User _user) _ = return ()
incCoucou (IRC.Server _) _ = return ()


sendCoucouCount :: IRC.Source Text -> Text -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendCoucouCount (IRC.Channel _ user) target mbNick = sendCoucouCount' user target mbNick
sendCoucouCount (IRC.User user) _ mbNick = sendCoucouCount' user user mbNick
sendCoucouCount (IRC.Server _) _ _ = return ()

sendCoucouCount' :: Text -> Text -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendCoucouCount' user target mbNick = do
    let coucouTarget = fromMaybe user mbNick
    state <- IRC.state
    socials <- liftIO $ STM.readTVarIO (T._socialDb state)
    case Map.lookup coucouTarget socials of
        Nothing -> return ()
        Just socialRec -> do
            let count = T._coucous socialRec
            let payload = coucouTarget <> " est un coucouteur niveau " <> (pack . show $ count)
            sendPrivMsg target payload
