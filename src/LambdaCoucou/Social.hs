{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Social where

import Prelude hiding (null)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate, null)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
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
        , T._toTell = V.empty
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

registerTell :: IRC.UnicodeEvent
             -> Text
             -> Text
             -> Maybe Integer
             -> IRC.StatefulIRC T.BotState (Maybe Text)
registerTell ev nick payload mbDelay =
    case IRC._source ev of
        IRC.Server _ -> return Nothing -- shouldn't happen, but who knows
        IRC.User sender ->
            if sender == nick
                then register' Nothing sender
                else return Nothing
        IRC.Channel chanName sender -> register' (Just chanName) sender
  where
    register' chan sender = do
        state <- IRC.state
        now <- Time.toSeconds <$> liftIO Time.getCurrentTime
        let delay = fromMaybe 0 $ (+ now) <$> mbDelay
        liftIO $
            STM.atomically $
            do socials <- STM.readTVar (T._socialDb state)
               let socials' = addToTell chan sender nick payload delay socials
               STM.writeTVar (T._socialDb state) socials'
               updateSocials (T._writerQueue state) socials'
        return $ Just "Ok."

addToTell :: Maybe Text -> Text -> Text -> Text -> Integer -> T.SocialRecords -> T.SocialRecords
addToTell chan sender nick payload delay = Map.adjust appendToTell nick
  where
    val =
        T.ToTell
        { T._toTellMsg = payload
        , T._toTellTs = delay
        , T._toTellFrom = sender
        , T._toTellOnChannel = chan
        }
    appendToTell social =
        social
        { T._toTell = V.snoc (T._toTell social) val
        }

sendTellMessages :: IRC.UnicodeEvent -> IRC.StatefulIRC T.BotState ()
sendTellMessages ev = case IRC._source ev of
    IRC.Server _ -> return ()
    IRC.User _nick -> return () -- ignore priv message atm
    IRC.Channel _chanName nick -> do
        state <- IRC.state
        now <- Time.toSeconds <$> liftIO Time.getCurrentTime
        messages <- liftIO $ STM.atomically $ do
            socials <- STM.readTVar (T._socialDb state)
            case Map.lookup nick socials of
                Nothing -> return V.empty
                Just social -> do
                    let (msgToTell, notYet) = V.partition (\t -> T._toTellTs t <= now) (T._toTell social)
                    let social' = social {T._toTell = notYet}
                    let socials' = Map.insert nick social' socials
                    STM.writeTVar (T._socialDb state) socials'
                    updateSocials (T._writerQueue state) socials'
                    return msgToTell
        liftIO $ print $ "sending tell messages: " <> show messages
        forM_ messages $ \toTell -> do
            let msg = "(From " <> T._toTellFrom toTell <> "): " <> T._toTellMsg toTell
            IRC.reply ev msg
