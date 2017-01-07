{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Social where

import Prelude hiding (null)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate, null)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified Data.HashMap.Strict as Map
import qualified Data.DateTime as Time

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Db (updateSocials)

makeDefaultUsr :: IO T.SocialRecord
makeDefaultUsr = do
    t <- Time.toSeconds <$> Time.getCurrentTime
    return
        T.SocialRecord
        { T._coucous = 0
        , T._lastSeen = t
        }

incCoucou :: IRC.Source Text -> IRC.StatefulIRC T.BotState ()
incCoucou (IRC.Channel _chanName user) = do
    liftIO . print $ "incCoucou count for " <> user <> " " <> pack (show T.CoucouCmdIncCoucou)
    state <- IRC.state
    defaultUsr <- liftIO makeDefaultUsr
    let socialT = T._socialDb state
    liftIO $
        STM.atomically $
        do social <- STM.readTVar socialT
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
incCoucou (IRC.User _user) = return ()
incCoucou (IRC.Server _) = return ()


getCoucouCount :: IRC.Source Text -> Maybe Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getCoucouCount (IRC.Channel _ user) mbNick = getCoucouCount' user mbNick
getCoucouCount (IRC.User user) mbNick = getCoucouCount' user mbNick
getCoucouCount (IRC.Server _) _ = return Nothing

getCoucouCount' :: Text -> Maybe Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getCoucouCount' user mbNick = do
    let coucouTarget = fromMaybe user mbNick
    state <- IRC.state
    socials <- liftIO $ STM.readTVarIO (T._socialDb state)
    case Map.lookup coucouTarget socials of
        Nothing -> return Nothing
        Just socialRec -> do
            let count = T._coucous socialRec
            let payload = coucouTarget <> " est un coucouteur niveau " <> (pack . show $ count)
            return $ Just payload
            -- IRC.reply ev payload

updateLastSeen :: IRC.Source Text -> IRC.StatefulIRC T.BotState ()
updateLastSeen (IRC.User nick) = updateLastSeen' nick
updateLastSeen (IRC.Channel _ nick) = updateLastSeen' nick
updateLastSeen _ = return ()

updateLastSeen' :: Text -> IRC.StatefulIRC T.BotState ()
updateLastSeen' nick = do
    state <- IRC.state
    defaultUsr <- liftIO makeDefaultUsr
    t <- Time.toSeconds <$> liftIO Time.getCurrentTime
    liftIO $
        STM.atomically $
        do socials <- STM.readTVar (T._socialDb state)
           let usr' =
                   case Map.lookup nick socials of
                       Nothing -> defaultUsr
                       Just socialRecord ->
                           socialRecord
                           { T._lastSeen = t
                           }
           let socials' = Map.insert nick usr' socials
           STM.writeTVar (T._socialDb state) socials'
           updateSocials (T._writerQueue state) socials'

prettyPlural :: Integer -> Text -> Text
prettyPlural 0 _ = ""
prettyPlural 1 t = "1 " <> t
prettyPlural n t = pack (show n) <> " " <> t <> "s"

prettyPrintDiffTime :: Integer -> Text
prettyPrintDiffTime t =
    let mt = 60
        ht = mt * 60
        dt = ht * 24
        d = t `div` dt
        h = (t - d * dt) `div` ht
        m = (t - d * dt - h * ht) `div` mt
        s = t `mod` 60
    in if t == 0
           then "just now"
           else intercalate
                    ", "
                    (filter
                         (not . null)
                         [prettyPlural d "day", prettyPlural h "hour", prettyPlural m "minute", prettyPlural s "second"]) <>
                " ago."

getLastSeen :: Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getLastSeen nick = do
    state <- IRC.state
    socials <- liftIO $ STM.readTVarIO (T._socialDb state)
    now <- Time.toSeconds <$> liftIO Time.getCurrentTime
    case Map.lookup nick socials of
        Nothing -> return (Just "Who ?")
        Just record -> do
            let diff = now - T._lastSeen record
            let payload = nick <> " has been seen " <> prettyPrintDiffTime diff
            return (Just payload)
