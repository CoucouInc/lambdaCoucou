{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module LambdaCoucou.TwitchTypes where

import RIO
import qualified RIO.Time as Time
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'

import qualified Control.Concurrent.MVar as MVar
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON
import qualified Servant.API as SAPI
import qualified Control.Concurrent.STM.TBMChan as Chan

newtype UserLogin = UserLogin {getUserLogin :: Text}
  deriving (Show, Eq)
  deriving newtype (JSON.FromJSON)

newtype ClientID = ClientID {getClientID :: Text}
  deriving stock (Show, Eq)
  deriving newtype (SAPI.ToHttpApiData)

newtype ClientSecret = ClientSecret {getClientSecret :: Text}
  deriving newtype (SAPI.ToHttpApiData)

newtype ClientAuthToken = ClientAuthToken {getClientAuthToken :: Text}
  deriving stock (Show, Eq)
  deriving newtype (SAPI.ToHttpApiData, JSON.FromJSON)

newtype UserID = UserID {getUserID :: Text}
  deriving (Show, Eq)
  deriving newtype (JSON.FromJSON)

data StreamData
  = StreamData
      { sdTitle :: Text,
        sdStartedAt :: Text,
        sdUserID :: UserID,
        sdUserName :: UserLogin
      }
  deriving (Show, Eq)

instance JSON.FromJSON StreamData where
  parseJSON = JSON.withObject "StreamData" $ \o -> do
    sdTitle <- o .: "title"
    sdStartedAt <- o .: "started_at"
    sdUserID <- o .: "user_id"
    sdUserName <- o .: "user_name"
    pure StreamData {..}

data User
  = User
      { usrChannelDescription :: Text,
        usrDisplayName :: Text,
        usrLoginName :: UserLogin,
        usrID :: UserID,
        usrViewCount :: Int
      }
  deriving (Show, Eq)

instance JSON.FromJSON User where
  parseJSON = JSON.withObject "twitch user" $ \o -> do
    usrChannelDescription <- o .: "description"
    usrDisplayName <- o .: "display_name"
    usrID <- o .: "id"
    usrLoginName <- o .: "login"
    usrViewCount <- o .: "view_count"
    pure $ User {..}

newtype SingleTwitchResponse a = SingleTwitchResponse {getSingleTwitchResponse :: a}
  deriving stock (Show, Eq)

instance (JSON.FromJSON a) => JSON.FromJSON (SingleTwitchResponse a) where
  parseJSON = JSON.withObject "twitch single response" $ \o -> do
    ds <- o .: "data"
    JSON.withArray "data" dataParser ds
    where
      dataParser arr = case V.length arr of
        0 -> fail $ "No data received: " <> show arr
        1 -> SingleTwitchResponse <$> JSON.parseJSON (V'.head arr)
        _ -> fail $ "Expected one datum but got: " <> show arr

data StreamState
  = StreamState
      { twitchClientID :: Text,
        twitchClientSecret :: Text,
        twitchClientAuthToken :: ClientAuthToken,
        twitchStreams :: Vector TwitchStream
      }
  deriving (Show, Eq)

data TwitchStream
  = TwitchStream
      { twitchUserLogin :: UserLogin,
        -- | irc nick for the corresponding streamer
        ircNick :: Text,
        -- | channels to notify when the stream goes online
        ircChannels :: Vector Text,
        -- | used as a flag to see if a stream just went live. The date isn't parsed and is treated as an opaque string
        lastStartDate :: Maybe Text
      }
  deriving (Show, Eq)

data ClientCredentialsResponse
  = ClientCredentialsResponse
      { ccrClientAuthToken :: ClientAuthToken,
        ccrExpiresInSeconds :: Int
      }
  deriving (Show, Eq)

instance JSON.FromJSON ClientCredentialsResponse where
  parseJSON = JSON.withObject "ClientCredentials" $ \o ->
    ClientCredentialsResponse <$> o .: "access_token" <*> o .: "expires_in"

data ClientCredentials
  = ClientCredentials
      { ccClientAuthToken :: ClientAuthToken,
        ccExpiresAt :: Time.UTCTime
      }
  deriving (Show, Eq)

data HubMode = Subscribe | Unsubscribe
  deriving (Show, Eq)

instance SAPI.FromHttpApiData HubMode where
  parseQueryParam = \case
    "subscribe" -> Right Subscribe
    "unsubscribe" -> Right Unsubscribe
    other -> Left other

instance SAPI.ToHttpApiData HubMode where
  toQueryParam = \case
    Subscribe -> "subscribe"
    Unsubscribe -> "unsubscribe"

instance JSON.ToJSON HubMode where
  toJSON = \case
    Subscribe -> "subscribe"
    Unsubscribe -> "unsubscribe"


newtype HubChallenge = HubChallenge {getHubChallenge :: Text}
  deriving stock (Show, Eq)
  deriving newtype (SAPI.FromHttpApiData, SAPI.ToHttpApiData, SAPI.MimeRender SAPI.PlainText)

newtype HubLeaseSeconds = HubLeaseSeconds {getHubLeaseSeconds :: Int}
  deriving stock (Show, Eq)
  deriving newtype (SAPI.FromHttpApiData, SAPI.ToHttpApiData, JSON.ToJSON)

newtype HubTopic = HubTopic {getHubTopic :: Text}
  deriving stock (Show, Eq)
  deriving newtype (SAPI.FromHttpApiData, SAPI.ToHttpApiData, JSON.ToJSON)

data StreamNotification
  = StreamOffline
  | StreamOnline StreamData
  deriving (Show, Eq)


instance JSON.FromJSON StreamNotification where
  parseJSON raw = onlineParser raw <|> offlineParser raw

    where
      onlineParser v = do
        (SingleTwitchResponse x) <- JSON.parseJSON v
        pure $ StreamOnline x
      offlineParser = JSON.withObject "StreamOfflineNotification" $ \o -> do
        ds <- o .: "data"
        JSON.withArray "data" (\a ->
          if V.null a
            then pure StreamOffline
            else fail $ "Expected empty data array but got: " <> show a
          ) ds

data SubscribeParams
  = SubscribeParams
      { spCallback :: Text,
        spMode :: HubMode,
        spTopic :: HubTopic,
        spLease :: HubLeaseSeconds
      }
  deriving (Show, Eq)

instance JSON.ToJSON SubscribeParams where
  toJSON sp =
    JSON.object
      [ "hub.callback" .= spCallback sp,
        "hub.mode" .= spMode sp,
        "hub.topic" .= spTopic sp,
        "hub.lease_seconds" .= spLease sp
      ]


data Lease
  = Lease
      { leaseTopic :: HubTopic,
        leaseExpiresAt :: Time.UTCTime
      }
  deriving (Show, Eq)

data StreamWatcherSpec = StreamWatcherSpec
  { swsTwitchUserLogin :: UserLogin
  , swsIRCNick :: Text
  , swsIRCChan :: Text
  }

data ClientEnv
  = ClientEnv
      { ceClientID :: ClientID,
        ceClientSecret :: ClientSecret,
        ceClientCredentials :: MVar.MVar (Maybe ClientCredentials),
        ceWebhookServerPort :: Int,
        ceNotificationLeases :: MVar.MVar [Lease],
        ceNotifChan :: Chan.TBMChan StreamNotification
      }
