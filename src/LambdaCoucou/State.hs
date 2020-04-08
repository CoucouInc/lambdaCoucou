{-# LANGUAGE StrictData #-}

module LambdaCoucou.State where

import RIO
import qualified Data.RingBuffer as RB

newtype YoutubeAPIKey
  = YoutubeAPIKey {getYoutubeAPIKey :: Text}
  deriving (IsString) via Text

instance Show YoutubeAPIKey where
  show _ = "YoutubeAPIKey <hidden>"

newtype ChannelName = ChannelName {getChannelName :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Ord)

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
        csSQLitePath :: FilePath
      }
  deriving (Generic)

initialState :: YoutubeAPIKey -> FilePath -> CoucouState
initialState key fp =
  CoucouState
    { csCounter = 0,
      csChannels = mempty,
      csYtAPIKey = key,
      csSQLitePath = fp
    }
