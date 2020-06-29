{-# OPTIONS_GHC -Wno-unused-matches #-}

module LambdaCoucou.Remind where

-- import Database.SQLite.Simple (NamedParam (..))

-- import qualified Database.SQLite.Simple.Ok as SQL

import qualified Data.Maybe as Mb
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import qualified RIO.Time as Time
import Say (sayShow, sayString)

data RemindSpec
  = RemindDuration TimeSpec
  | RemindTime TimeSpec
  | RemindTonight
  | RemindWeekDay (Maybe (Int, Maybe Int))
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
  { rrTargetChan :: LC.St.ChannelName,
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

deleteReminder :: SQL.Connection -> Int -> IO ()
deleteReminder conn reminderId =
  SQL.execute
    conn
    "DELETE FROM reminders WHERE id=?"
    (SQL.Only reminderId)

createTable :: SQL.Connection -> IO ()
createTable conn =
  SQL.execute_
    conn
    "CREATE TABLE IF NOT EXISTS reminders(\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \target_chan TEXT NOT NULL,\
    \nick TEXT NOT NULL,\
    \created_at TEXT NOT NULL,\
    \remind_at TEXT NOT NULL,\
    \content TEXT NOT NULL\
    \)"

test :: IO ()
test = SQL.withConnection "coucou.sqlite" $ \conn -> do
  createTable conn
  now <- Time.getCurrentTime
  sayString $ "now: " <> show now
  -- let rr =
  --       RemindRecord
  --         { rrTargetChan = LC.St.ChannelName "#chan",
  --           rrNick = "nick",
  --           rrCreatedAt = now,
  --           rrRemindAt = now,
  --           rrContent = "content"
  --         }
  -- createReminder conn rr
  let beforeTs = Time.addUTCTime 10 now
  sayString $ "before: " <> show beforeTs
  rs <- getRemindersBefore beforeTs conn
  forM_ rs sayShow

-- SQL.execute conn "INSERT INTO reminders (name) VALUES (?)" (TestRow "coucou")

-- data ReminderRow = ReminderRow
--   {
--   }

remindCommandHandler ::
  LC.St.ChannelName ->
  Text ->
  RemindSpec ->
  Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
remindCommandHandler chanName nick spec text = do
  now <- Time.getCurrentTime
  connPath <- St.gets LC.St.csSQLitePath
  sayString $ "remind spec: " <> show spec
  let remindAt = timeFromSpec now spec
  let reminder =
        RemindRecord
          { rrTargetChan = chanName,
            rrNick = nick,
            rrCreatedAt = now,
            rrRemindAt = remindAt,
            rrContent = text
          }
  liftIO $ SQL.withConnection connPath $ \conn -> createReminder conn reminder
  pure $ Just $ "Saved! Will remind at " <> prettyTs remindAt

timeFromSpec :: Time.UTCTime -> RemindSpec -> Time.UTCTime
timeFromSpec now = \case
  RemindDuration ts -> timeFromDuration ts
  RemindTime ts -> timeFromGivenTime ts
  RemindTonight -> error "wip"
  RemindWeekDay _ -> error "wip"
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
          nowSec = Time.diffTimeToPicoseconds (Time.utctDayTime now) `div` (10 ^ 12)
          (nh, rest) = nowSec `divMod` 3600
          nMin = rest `div` 60
          dayTime = Time.secondsToDiffTime $
            (Mb.maybe nh fromIntegral $ dsHour ts) * 3600
              + (Mb.maybe nMin fromIntegral $ dsMinute ts) * 60
       in Time.UTCTime
            { Time.utctDay = day',
              Time.utctDayTime = dayTime
            }

-- TODO: currently there's a bug. If a reminder should be sent to a channel
-- but the bot hasn't joined that channel yet, it will be sent into the
-- void and be lost.
-- Need to find a way to wait until the bot is connected to the right channels.
-- Also need to autojoin channels or discard messages from old channels (probably the later)
processReminders :: IRC.C.IRC LC.St.CoucouState ()
processReminders = do
  let intervalInSec = (10 :: Integer)
  connPath <- St.gets LC.St.csSQLitePath
  liftIO $ SQL.withConnection connPath createTable
  forever $ do
    now <- liftIO Time.getCurrentTime
    msgToSend <- liftIO $ processReminders' connPath intervalInSec now
    forM_ msgToSend $ \(chan, msg) ->
      IRC.C.send $ IRC.Ev.Privmsg (LC.St.getChannelName chan) (Right msg)
    threadDelay (fromInteger intervalInSec * 10 ^ 6)

processReminders' :: FilePath -> Integer -> Time.UTCTime -> IO [(LC.St.ChannelName, Text)]
processReminders' connPath intervalSec now = do
  let beforeTime = Time.addUTCTime (fromInteger $ intervalSec) now
  reminders <- SQL.withConnection connPath $ \conn -> do
    rs <- getRemindersBefore beforeTime conn
    -- not quite safe to delete the reminders before they are sent out but I'm not going
    -- to bother for this project
    mapM_ sayShow rs
    mapM_ (\(Entity i _) -> deleteReminder conn i) rs
    pure rs
  pure $ fmap formatReminder reminders

formatReminder :: Entity Int RemindRecord -> (LC.St.ChannelName, Text)
formatReminder (Entity reminderId rr) =
  let txt = rrNick rr <> ": " <> rrContent rr <> " (reminder created at " <> prettyTs (rrCreatedAt rr) <> ")"
   in (rrTargetChan rr, txt)

prettyTs :: Time.UTCTime -> Text
prettyTs = T.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M %Z"

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
