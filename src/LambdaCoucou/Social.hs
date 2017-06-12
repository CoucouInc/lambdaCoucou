{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Social where

import Prelude hiding (null)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate, null)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as Chan
import qualified Network.IRC.Client as IRC
import qualified Data.HashMap.Strict as Map
import qualified Data.DateTime as Time

import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Db as DB

makeDefaultUsr :: IO T.SocialRecord
makeDefaultUsr = do
    t <- Time.toSeconds <$> Time.getCurrentTime
    return
        T.SocialRecord
        { T._coucous = 0
        , T._lastSeen = t
        , T._toTell = V.empty
        , T._reminders = V.empty
        }

incCoucou :: IRC.Source Text -> IRC.StatefulIRC T.BotState ()
incCoucou (IRC.Channel _chanName user) = do
    defaultUsr <- liftIO makeDefaultUsr
    void $ updateSocialRecords (updateCoucou defaultUsr user)

-- TODO decrease coucou count if sent from private message
incCoucou (IRC.User _user) = return ()
incCoucou (IRC.Server _) = return ()

updateCoucou :: T.SocialRecord -> Text -> T.SocialRecords -> T.SocialRecords
updateCoucou defaultUsr = Map.alter coucouPlusPlus
  where
    coucouPlusPlus (Just social) = Just $ social { T._coucous = succ (T._coucous social) }
    coucouPlusPlus Nothing = Just $ defaultUsr { T._coucous = succ (T._coucous defaultUsr) }


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

updateLastSeen :: IRC.Source Text -> IRC.StatefulIRC T.BotState ()
updateLastSeen (IRC.User nick) = updateLastSeen' nick
updateLastSeen (IRC.Channel _ nick) = updateLastSeen' nick
updateLastSeen _ = return ()

updateLastSeen' :: Text -> IRC.StatefulIRC T.BotState ()
updateLastSeen' nick = do
    ts <- Time.toSeconds <$> liftIO Time.getCurrentTime
    defaultUsr <- liftIO makeDefaultUsr
    let updateLastSeenTs (Just usr) = Just $ usr { T._lastSeen = ts }
        updateLastSeenTs Nothing = Just defaultUsr
    void $ updateSocialRecords (Map.alter updateLastSeenTs nick)

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
                         [ prettyPlural d "day"
                         , prettyPlural h "hour"
                         , prettyPlural m "minute"
                         , prettyPlural s "second"
                         ]) <>
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
             -> Maybe T.Timestamp
             -> IRC.StatefulIRC T.BotState (Maybe Text)
registerTell ev nick msg mbDelay = withChannelMessage Nothing ev $ \chan sender -> do
    toTell <- liftIO $ makeTell chan sender msg mbDelay
    updateSocialRecords (addToTell nick toTell)
    return $ Just "Ok"

registerRemind :: IRC.UnicodeEvent
               -> Text
               -> Text
               -> Maybe T.Timestamp
               -> IRC.StatefulIRC T.BotState (Maybe Text)
registerRemind ev nick msg mbDelay = case IRC._source ev of
    IRC.Server _ -> return Nothing
    IRC.User usr -> register' usr usr
    IRC.Channel chan sender -> register' chan sender
  where
    register' chan sender = do
        let nick' = if nick == "me" then sender else nick
        reminder <- liftIO $ makeReminder chan sender msg mbDelay
        updateSocialRecords (addReminder nick' reminder)
        sendReminder nick' reminder
        return $ Just "Ok"

addToTell :: Text -> T.ToTell -> T.SocialRecords -> T.SocialRecords
addToTell nick toTell = Map.adjust appendToTell nick
  where
    appendToTell social =
        social
        { T._toTell = V.snoc (T._toTell social) toTell
        }

addReminder :: Text -> T.Remind -> T.SocialRecords -> T.SocialRecords
addReminder nick reminder = Map.adjust appendReminders nick
    where
        appendReminders social =
            social
            { T._reminders = V.snoc (T._reminders social) reminder
            }

makeTell :: Text -> Text -> Text -> Maybe T.Timestamp -> IO T.ToTell
makeTell chan sender msg mbDelay = do
    ts <- makeTs mbDelay
    return
        T.ToTell
        { T._toTellMsg = msg
        , T._toTellTs = ts
        , T._toTellFrom = sender
        , T._toTellOnChannel = chan
        }

makeReminder :: Text -> Text -> Text -> Maybe T.Timestamp -> IO T.Remind
makeReminder chan sender msg mbDelay = do
    ts <- makeTs mbDelay
    return
        T.Remind
        { T._remindMsg = msg
        , T._remindTs = ts
        , T._remindFrom = sender
        , T._remindOnChannel = chan
        }

-- make a timestamp from a delay
makeTs :: Maybe T.Timestamp -> IO T.Timestamp
makeTs delay = do
    now <- Time.toSeconds <$> Time.getCurrentTime
    return $ now + fromMaybe 0 delay

updateSocialRecords
    :: (T.SocialRecords -> T.SocialRecords)
    -> IRC.StatefulIRC T.BotState T.SocialRecords
updateSocialRecords updateSocialAction = do
        state <- IRC.state
        liftIO $
            STM.atomically $
            do socials <- STM.readTVar (T._socialDb state)
               let socials' = updateSocialAction socials
               STM.writeTVar (T._socialDb state) socials'
               DB.updateSocials (T._writerQueue state) socials'
               return socials'

withChannelMessage
    :: Monad m
    => a -> IRC.UnicodeEvent -> (Text -> Text -> m a) -> m a
withChannelMessage retval ev action =
    case IRC._source ev of
        IRC.Server _ -> return retval -- shouldn't happen, but who knows
        IRC.User _ -> return retval
        IRC.Channel chanName sender -> action chanName sender

sendTellMessages :: IRC.UnicodeEvent -> IRC.StatefulIRC T.BotState ()
sendTellMessages ev =
    withChannelMessage () ev $
    \_chanName nick -> do
        state <- IRC.state
        now <- Time.toSeconds <$> liftIO Time.getCurrentTime
        messages <-
            liftIO $
            STM.atomically $
            do socials <- STM.readTVar (T._socialDb state)
               case Map.lookup nick socials of
                   Nothing -> return V.empty
                   Just social -> do
                       let (msgToTell, notYet) =
                               V.partition (\t -> T._toTellTs t <= now) (T._toTell social)
                       let social' =
                               social
                               { T._toTell = notYet
                               }
                       let socials' = Map.insert nick social' socials
                       STM.writeTVar (T._socialDb state) socials'
                       DB.updateSocials (T._writerQueue state) socials'
                       return msgToTell
        forM_ messages $
            \toTell -> do
                let msg = "(From " <> T._toTellFrom toTell <> ") " <> nick <> ": " <> T._toTellMsg toTell
                -- TODO take care of the channel parameter for toTell (?)
                IRC.reply ev msg

sendReminder :: Text -> T.Remind -> IRC.StatefulIRC T.BotState ()
sendReminder nick reminder = do
    now <- liftIO $ Time.toSeconds <$> Time.getCurrentTime
    let delayS = max 0 (T._remindTs reminder - now)
    let delayMicroS = fromInteger $ 1000000 * delayS
    state <- IRC.state
    conn <- IRC.getConnectionConfig <$> IRC.ircState
    let sendQueue = IRC._sendqueue conn
    let payload = "(From " <> T._remindFrom reminder <> ") " <> nick <> ": " <> T._remindMsg reminder
    let ircMessage = IRC.Privmsg (T._remindOnChannel reminder) (Right payload)
    liftIO $ print $ "Sending irc message: " <> show ircMessage
    void $ liftIO $ forkIO $ do
        threadDelay delayMicroS
        now' <- liftIO $ Time.toSeconds <$> Time.getCurrentTime
        STM.atomically $ do
            Chan.writeTBMChan sendQueue (encodeUtf8 <$> ircMessage)
            socials <- STM.readTVar (T._socialDb state)
            let socials' = cleanupReminders now' nick socials
            STM.writeTVar (T._socialDb state) socials'
            DB.updateSocials (T._writerQueue state) socials'

cleanupReminders :: T.Timestamp -> Text -> T.SocialRecords -> T.SocialRecords
cleanupReminders now =
    Map.adjust
        (\socials ->
              let reminders' = V.filter (\r -> T._remindTs r > now) (T._reminders socials)
                  socials' =
                      socials
                      { T._reminders = reminders'
                      }
              in socials')

setupReminders :: IRC.StatefulIRC T.BotState ()
setupReminders = do
    state <- IRC.state
    socials <- liftIO $ STM.readTVarIO (T._socialDb state)
    let withReminders = filter (not . V.null . T._reminders . snd) (Map.toList socials)
    forM_ withReminders $
        \(nick, social) -> forM_ (T._reminders social) $ \reminder -> sendReminder nick reminder
