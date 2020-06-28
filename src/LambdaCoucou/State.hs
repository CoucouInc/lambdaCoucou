{-# LANGUAGE StrictData #-}

module LambdaCoucou.State where

import RIO
import qualified Data.RingBuffer as RB
import qualified LambdaCoucou.TwitchTypes as Twitch
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.FromField as SQL

newtype YoutubeAPIKey
  = YoutubeAPIKey {getYoutubeAPIKey :: Text}
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

data ChannelState
  = ChannelState
      { cstUsers :: Set Text,
        cstType :: ChannelType,
        cstLastUrls :: RB.RingBuffer Vector Text
      }
  deriving (Generic)

data CoucouState
  = CoucouState
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
  pure $ CoucouState
    { csCounter = 0,
      csChannels = mempty,
      csYtAPIKey = key,
      csSQLitePath = fp,
      csTwitch = emptyTwitchState
    }
