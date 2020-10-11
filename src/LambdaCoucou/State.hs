{-# LANGUAGE StrictData #-}

module LambdaCoucou.State where

import qualified Data.RingBuffer as RB
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.TwitchTypes as Twitch
import RIO

newtype YoutubeAPIKey = YoutubeAPIKey {getYoutubeAPIKey :: Text}
  deriving (IsString) via Text

instance Show YoutubeAPIKey where
  show _ = "YoutubeAPIKey <hidden>"

newtype ChannelName = ChannelName {getChannelName :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Ord)
  deriving (SQL.ToField, SQL.FromField) via Text

data ChannelType
  = Secret
  | Private
  | Public
  deriving (Show, Eq)

data ChannelState = ChannelState
  { -- | keep track of users currently connected to the chan
    cstUsers :: Set Text,
    cstType :: ChannelType,
    -- | keep track of a limited number of urls posted in the chan
    cstLastUrls :: RB.RingBuffer Vector Text,
    -- | count of consecutive message starting with "coucou" (or a variation)
    cstCoucouCount :: Int,
    -- | keep track of the last 10 messages (used for sed replacements)
    cstLastMessages :: RB.RingBuffer Vector Text
  }
  deriving (Generic)

data CoucouState = CoucouState
  { csCounter :: Int,
    csYtAPIKey :: YoutubeAPIKey,
    csChannels :: Map ChannelName ChannelState,
    csSQLitePath :: FilePath,
    csTwitch :: MVar (Maybe Twitch.ClientEnv)
  }
  deriving (Generic)

initialState :: YoutubeAPIKey -> FilePath -> IO CoucouState
initialState key fp = do
  emptyTwitchState <- newMVar Nothing
  pure $
    CoucouState
      { csCounter = 0,
        csChannels = mempty,
        csYtAPIKey = key,
        csSQLitePath = fp,
        csTwitch = emptyTwitchState
      }
