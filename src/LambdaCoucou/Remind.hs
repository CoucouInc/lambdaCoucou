{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module LambdaCoucou.Remind where

import qualified Data.Maybe as Mb
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZ
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.Url as LC.Url
import qualified LambdaCoucou.UserSettings as LC.Settings
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Conduit.Internal as IRC.C
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import qualified RIO.Time as Time
import Say (sayShow, sayString)

data RemindCmd
  = Reminder RemindSpec Text
  | RemindList
  | RemindDelete Int
  deriving (Show, Eq)

data RemindSpec
  = RemindDuration TimeSpec
  | RemindTime TimeSpec
  | RemindTomorrow (Maybe (Int, Int))
  | RemindWeekDay Time.DayOfWeek (Maybe (Int, Int))
  deriving (Show, Eq)

data TimeSpec = TimeSpec
  { dsYear :: Maybe Int,
    dsMonth :: Maybe Int,
    dsDay :: Maybe Int,
    dsHour :: Maybe Int,
    dsMinute :: Maybe Int
  }
  deriving (Show, Eq)

data Entity i a = Entity i a
  deriving (Show, Eq)

instance (SQL.ToRow a, SQL.ToField i) => SQL.ToRow (Entity i a) where
  toRow (Entity i x) = SQL.toField i : SQL.toRow x

data RemindRecord = RemindRecord
  { rrTargetChan :: Maybe LC.St.ChannelName,
    rrNick :: Text,
    rrCreatedAt :: Time.UTCTime,
    rrRemindAt :: Time.UTCTime,
    rrContent :: Text
  }
  deriving (Show, Eq)

instance SQL.ToRow RemindRecord where
  toRow rr =
    [ SQL.toField (rrTargetChan rr),
      SQL.toField (rrNick rr),
      SQL.toField (rrCreatedAt rr),
      SQL.toField (rrRemindAt rr),
      SQL.toField (rrContent rr)
    ]

instance SQL.FromRow RemindRecord where
  fromRow =
    RemindRecord
      <$> SQL.field
      <*> SQL.field
      <*> SQL.field
      <*> SQL.field
      <*> SQL.field

instance (SQL.FromField i, SQL.FromRow a) => SQL.FromRow (Entity i a) where
  fromRow = do
    ((SQL.Only i) SQL.:. b) <- SQL.fromRow
    pure $ Entity i b

createReminder :: SQL.Connection -> RemindRecord -> IO ()
createReminder conn rr =
  SQL.execute
    conn
    ( "INSERT INTO reminders ("
        <> "target_chan, nick, created_at, remind_at, content"
        <> ") VALUES (?,?,?,?,?)"
    )
    rr

getRemindersBefore :: Time.UTCTime -> SQL.Connection -> IO [Entity Int RemindRecord]
getRemindersBefore beforeTime conn =
  SQL.query
    conn
    "SELECT * FROM reminders WHERE remind_at <= ?"
    (SQL.Only beforeTime)

selectReminders :: Maybe LC.St.ChannelName -> Text -> SQL.Connection -> IO [Entity Int RemindRecord]
selectReminders mbChanName nick conn =
  SQL.query
    conn
    "SELECT * FROM reminders WHERE target_chan = ? AND nick = ? ORDER BY remind_at"
    (mbChanName, nick)

deleteReminder :: SQL.Connection -> Int -> IO ()
deleteReminder conn reminderId =
  SQL.execute
    conn
    "DELETE FROM reminders WHERE id=?"
    (SQL.Only reminderId)

createTable :: MonadIO m => FilePath -> m ()
createTable fp = liftIO $
  SQL.withConnection fp $ \conn ->
    SQL.execute_
      conn
      "CREATE TABLE IF NOT EXISTS reminders(\
      \id INTEGER PRIMARY KEY AUTOINCREMENT,\
      \target_chan TEXT,\
      \nick TEXT NOT NULL,\
      \created_at TEXT NOT NULL,\
      \remind_at TEXT NOT NULL,\
      \content TEXT NOT NULL\
      \)"

test :: IO ()
test = SQL.withConnection "coucou.sqlite" $ \conn -> do
  createTable "coucou.sqlite"
  now <- Time.getCurrentTime
  sayString $ "now: " <> show now
  let beforeTs = Time.addUTCTime 10 now
  sayString $ "before: " <> show beforeTs
  rs <- getRemindersBefore beforeTs conn
  forM_ rs sayShow

remindCommandHandler ::
  Maybe LC.St.ChannelName ->
  Text ->
  RemindCmd ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
remindCommandHandler mbChanName nick = \case
  Reminder spec text -> handleSetReminder mbChanName nick spec text
  RemindList -> handleListReminders mbChanName nick
  RemindDelete rId -> handleDeleteReminder mbChanName nick rId

handleSetReminder ::
  Maybe LC.St.ChannelName ->
  Text ->
  RemindSpec ->
  Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
handleSetReminder mbChanName nick spec text = do
  nowUtc <- Time.getCurrentTime
  connPath <- St.gets LC.St.csSQLitePath
  tz <- getUserTZ nick
  now <- Time.getCurrentTime
  let remindAt = timeFromSpec now tz spec
  let reminder =
        RemindRecord
          { rrTargetChan = mbChanName,
            rrNick = nick,
            rrCreatedAt = nowUtc,
            rrRemindAt = remindAt,
            rrContent = text
          }
  liftIO $ SQL.withConnection connPath $ \conn -> createReminder conn reminder
  pure $ Just $ "Saved! Will remind at " <> prettyTs tz remindAt

timeFromSpec :: Time.UTCTime -> TZ.TZ -> RemindSpec -> Time.UTCTime
timeFromSpec now tz = \case
  RemindDuration ts -> timeFromDuration ts
  RemindTime ts -> timeFromGivenTime ts
  RemindTomorrow ts -> tomorrowTime ts
  RemindWeekDay weekday ts -> weekdayTime weekday ts
  where
    day = Time.utctDay now
    timeFromDuration ts =
      let day' =
            Time.addGregorianYearsRollOver (fromIntegral $ fromMaybe 0 $ dsYear ts) $
              Time.addGregorianMonthsRollOver (fromIntegral $ fromMaybe 0 $ dsMonth ts) $
                Time.addDays (fromIntegral $ fromMaybe 0 $ dsDay ts) $ day
          dt = fromIntegral $ 3600 * (fromMaybe 0 $ dsHour ts) + 60 * (fromMaybe 0 $ dsMinute ts)
          now' = now {Time.utctDay = day'}
          result = Time.addUTCTime dt now'
       in result

    timeFromGivenTime ts =
      let (ny, nm, nd) = Time.toGregorian day
          day' =
            Time.fromGregorian
              (Mb.maybe ny fromIntegral $ dsYear ts)
              (fromMaybe nm $ dsMonth ts)
              (fromMaybe nd $ dsDay ts)
          dayTime = mkDayTime (dsHour ts) (dsMinute ts)
       in Time.UTCTime
            { Time.utctDay = day',
              Time.utctDayTime = dayTime
            }

    tomorrowTime mbTs =
      let (h, minutes) = case mbTs of
            Nothing -> (Nothing, Nothing)
            Just (a, b) -> (Just a, Just b)
          ts =
            TimeSpec
              { dsYear = Nothing,
                dsMonth = Nothing,
                dsDay = Nothing,
                dsHour = h,
                dsMinute = minutes
              }
          withTime = timeFromGivenTime ts
          d = Time.addDays 1 (Time.utctDay withTime)
       in withTime {Time.utctDay = d}

    weekdayTime wd ts =
      let dayOfWeek = Time.dayOfWeek day
          deltaDay = fromEnum wd - fromEnum dayOfWeek
          targetDay = Time.addDays (fromIntegral deltaDay) day
          (mbHour, mbMin) = case ts of
            Nothing -> (Nothing, Nothing)
            Just (a, b) -> (Just a, Just b)
          dayTime = mkDayTime mbHour mbMin
          newTime = Time.UTCTime {Time.utctDay = targetDay, Time.utctDayTime = dayTime}
          newTime' =
            if newTime <= now
              then newTime {Time.utctDay = Time.addDays 7 targetDay}
              else newTime
       in newTime'

    mkDayTime :: Maybe Int -> Maybe Int -> Time.DiffTime
    mkDayTime mbHour mbMin =
      let localNow = Time.utcToZonedTime (TZ.timeZoneForUTCTime tz now) now
          localTod = Time.localTimeOfDay $ Time.zonedTimeToLocalTime localNow
          nowH = Time.todHour localTod
          nowMin = Time.todMin localTod
          tzDelta = Time.timeZoneMinutes $ TZ.timeZoneForUTCTime tz now
          dayTime =
            Time.secondsToDiffTime $
              fromIntegral $
                (Mb.fromMaybe nowH mbHour) * 3600
                  + (Mb.fromMaybe nowMin mbMin) * 60
                  - tzDelta * 60
       in dayTime

-- TODO: currently there's a bug. If a reminder should be sent to a channel
-- but the bot hasn't joined that channel yet, it will be sent into the
-- void and be lost.
-- Need to find a way to wait until the bot is connected to the right channels.
-- Also need to autojoin channels or discard messages from old channels (probably the later)
processReminders :: IRC.C.IRC LC.St.CoucouState ()
processReminders = do
  let intervalInSec = (10 :: Integer)
  connPath <- St.gets LC.St.csSQLitePath
  forever $ do
    now <- liftIO Time.getCurrentTime
    msgToSend <- processReminders' connPath intervalInSec now
    forM_ msgToSend $ \(target, msg) -> do
      LC.Url.updateLastUrl (LC.St.ChannelName target) msg
      IRC.C.send $ IRC.Ev.Privmsg target (Right msg)
    threadDelay (fromInteger intervalInSec * 10 ^ 6)

processReminders' ::
  (St.MonadState LC.St.CoucouState m, MonadIO m) =>
  FilePath ->
  Integer ->
  Time.UTCTime ->
  m [(IRC.C.Target Text, Text)]
processReminders' connPath intervalSec now = do
  let beforeTime = Time.addUTCTime (fromInteger $ intervalSec) now
  reminders <- liftIO $
    SQL.withConnection connPath $ \conn -> do
      rs <- getRemindersBefore beforeTime conn
      -- not quite safe to delete the reminders before they are sent out but I'm not going
      -- to bother for this project
      mapM_ (\(Entity i _) -> deleteReminder conn i) rs
      pure rs

  -- there shouldn't be many reminders at any point in time, so doing naive queries
  -- for each reminders to get the TZ is fine
  withTz <- mapM (\e@(Entity _ rr) -> (e,) <$> getUserTZ (rrNick rr)) reminders
  pure $ fmap formatReminder withTz

formatReminder :: (Entity Int RemindRecord, TZ.TZ) -> (IRC.C.Target Text, Text)
formatReminder ((Entity reminderId rr), tz) =
  let txt =
        rrNick rr
          <> ": "
          <> rrContent rr
          <> " (reminder created at "
          <> prettyTs tz (rrCreatedAt rr)
          <> ")"
      target = maybe (rrNick rr) LC.St.getChannelName (rrTargetChan rr)
   in (target, txt)

prettyTs :: TZ.TZ -> Time.UTCTime -> Text
prettyTs tz utcTime =
  T.pack $
    Time.formatTime
      Time.defaultTimeLocale
      "%Y-%m-%d %H:%M %Z (%z)"
      (Time.utcToZonedTime (TZ.timeZoneForUTCTime tz utcTime) utcTime)

getUserTZ :: (St.MonadState LC.St.CoucouState m, MonadIO m) => Text -> m TZ.TZ
getUserTZ nick = do
  usrSettings <- LC.Settings.getUserSettings nick
  let tz = fromMaybe TZ.utcTZ do
        settings <- usrSettings
        usrTzLabel <- LC.Settings.usTimezone settings
        pure $ TZ.tzByLabel usrTzLabel
  pure tz

handleListReminders ::
  Maybe LC.St.ChannelName ->
  Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
handleListReminders mbChanName nick = do
  connPath <- St.gets LC.St.csSQLitePath
  reminders <- liftIO $ SQL.withConnection connPath (selectReminders mbChanName nick)
  tz <- getUserTZ nick
  let result
        | null reminders = "No reminder set"
        | length reminders <= 3 = T.intercalate " − " (map (shortForm tz) reminders)
        | otherwise =
          "You have " <> tshow (length reminders) <> " reminders. "
            <> T.intercalate " − " (map (shortForm tz) $ take 3 reminders)
  pure $ Just result

shortForm :: TZ.TZ -> Entity Int RemindRecord -> Text
shortForm tz (Entity rId rr) =
  "(" <> tshow rId <> ") at "
    <> prettyTs tz (rrRemindAt rr)
    <> ": "
    <> ellipsis (rrContent rr)
  where
    ellipsis txt = if T.length txt > 30 then T.take 29 txt <> "…" else txt

handleDeleteReminder ::
  Maybe LC.St.ChannelName ->
  Text ->
  Int ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
handleDeleteReminder mbChanName nick rId = do
  connPath <- St.gets LC.St.csSQLitePath
  hasBeenDeleted <- liftIO $
    SQL.withConnection connPath $ \conn -> do
      reminders <- selectReminders mbChanName nick conn
      if rId `elem` (map (\(Entity i _) -> i) reminders)
        then do
          deleteReminder conn rId
          pure True
        else pure False
  pure $
    Just $
      if hasBeenDeleted
        then "Reminder deleted"
        else case mbChanName of
          Nothing -> "No reminder with ID " <> tshow rId
          Just chanName ->
            "No reminder with ID "
              <> tshow rId
              <> " for user "
              <> nick
              <> " in chan "
              <> (LC.St.getChannelName chanName)

{-

&remind in <duration>
&remind at <time>

&remind <day of the week> [at <time>]
&remind demain [at <time>]
&remind ce soir (20:00 UTC)

duration = <y> (year(s)|y) <m> (month(s)|m) <d> (day(s)|d) <h> (hour(s)|h) <m> (minute(s)|min)
if 0, field omitted

time
  = DD MM YYYY [HH:MM] (assume current time)
  | DD MM [HH:MM] (assume current year, current time if not provided)
  | HH:MM

-}
