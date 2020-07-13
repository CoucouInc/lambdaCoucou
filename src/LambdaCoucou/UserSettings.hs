{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.UserSettings
  ( SettingsCmd (..),
    createTable,
    settingsCommandHandler,
  )
where

import Data.Generics.Product.Fields as GPF
import qualified Data.Time.Zones.All as TZ
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import Say

data SettingsCmd
  = UserTZ (Maybe Text)
  | Display
  deriving (Show, Eq)

data UserSettings = UserSettings
  { usNick :: Text,
    usTimezone :: Maybe TZ.TZLabel
  }
  deriving (Show)

instance SQL.ToField TZ.TZLabel where
  toField label = SQL.toField (TZ.toTZName label)

instance SQL.FromField TZ.TZLabel where
  fromField raw = do
    bs <- SQL.fromField raw
    case TZ.fromTZName bs of
      Nothing -> fail $ "Cannot parse " <> T.unpack (decodeUtf8Lenient bs) <> " as timezone label"
      Just x -> pure x

instance SQL.ToRow UserSettings where
  toRow us = [SQL.toField (usNick us), SQL.toField (usTimezone us)]

instance SQL.FromRow UserSettings where
  fromRow = UserSettings <$> SQL.field <*> SQL.field

createTable :: MonadIO m => FilePath -> m ()
createTable fp = liftIO $
  SQL.withConnection fp $ \conn ->
    SQL.execute_
      conn
      "CREATE TABLE IF NOT EXISTS user_settings(\
      \nick TEXT PRIMARY KEY,\
      \timezone TEXT\
      \)"

getUserSettings ::
  ( MonadIO m,
    GPF.HasField' "csSQLitePath" r FilePath,
    St.MonadState r m
  ) =>
  Text ->
  m (Maybe UserSettings)
getUserSettings nick = do
  fp <- St.gets (^. GPF.field' @"csSQLitePath")
  results <- liftIO $
    SQL.withConnection fp $ \conn ->
      SQL.query conn "SELECT * FROM user_settings WHERE nick = ?" (SQL.Only nick)
  case results of
    [] -> pure Nothing
    [x] -> pure (Just x)
    _ -> throwString ("Primary key violation for nick " <> T.unpack nick)

settingsCommandHandler ::
  ( MonadIO m,
    GPF.HasField' "csSQLitePath" r FilePath,
    St.MonadState r m
  ) =>
  Text ->
  SettingsCmd ->
  m (Maybe Text)
settingsCommandHandler nick cmd = do
  fp <- St.gets (^. GPF.field' @"csSQLitePath")
  case cmd of
    UserTZ Nothing -> do
      liftIO $
        SQL.withConnection fp $ \conn ->
          SQL.execute
            conn
            "UPDATE user_settings SET timezone=NULL WHERE nick = ?"
            (SQL.Only nick)
      pure $ Just $ "Timezone preference removed."
    UserTZ (Just rawTZ) -> case TZ.fromTZName (encodeUtf8 rawTZ) of
      Nothing -> pure $ Just $ "Invalid timezone: " <> rawTZ
      Just tzLabel -> do
        let setting =
              UserSettings
                { usNick = nick,
                  usTimezone = Just tzLabel
                }
        liftIO $
          SQL.withConnection fp $ \conn ->
            SQL.execute
              conn
              "INSERT INTO user_settings(nick,timezone) VALUES (?,?) \
              \ON CONFLICT(nick) DO UPDATE SET timezone=excluded.timezone"
              setting
        say $ "Setting tz with " <> rawTZ
        pure $ Just $ "Timezone set to " <> rawTZ <> "."
    Display -> do
      settings <- liftIO $
        SQL.withConnection fp $ \conn ->
          SQL.query
            conn
            "SELECT * FROM user_settings WHERE nick = ?"
            (SQL.Only nick)
      case settings of
        [] -> pure $ Just $ "No settings saved for " <> nick
        [x] -> pure $ Just $ prettySetting x
        _ -> pure $ Just $ "Primary key violation for " <> nick

prettySetting :: UserSettings -> Text
prettySetting UserSettings {usNick, usTimezone} =
  "Settings for " <> usNick <> " : "
    <> maybe
      "no timezone preference"
      (\tz -> "timezone: " <> decodeUtf8Lenient (TZ.toTZName tz))
      usTimezone

{-

set tz europe/london
unset tz

set foo 3
unset foo

data Cmd
  = Set Key KeyArgs
  | Unset Key

Thanks to Tom for the gist for some type level neat trick using a dependent map
https://gist.github.com/i-am-tom/0fb414ebc37dcbb3994b8a94634d5038
I ended up using a simple ADT with a Maybe for Set/Unset though. Keep that simple
-}

-- data TestState = TestState {csSQLitePath :: FilePath} deriving (Generic)
--
-- test :: IO ()
-- test = do
--   let fp = "coucou.sqlite"
--   let st = TestState fp
--   createTable fp
--   flip St.evalStateT st $ do
--     getUserSettings "testNick" >>= sayShow
--     settingsCommandHandler "testNick" (UserTZ $ Just "Europe/Paris") >>= sayShow
--     getUserSettings "testNick" >>= sayShow
--     settingsCommandHandler "testNick" (UserTZ Nothing) >>= sayShow
--     getUserSettings "testNick" >>= sayShow
--   pure ()
