{-# LANGUAGE StrictData #-}

module LambdaCoucou.State where

import RIO

newtype YoutubeAPIKey
  = YoutubeAPIKey { getYoutubeAPIKey :: Text }
  deriving IsString via Text

instance Show YoutubeAPIKey where
  show _ = "YoutubeAPIKey <hidden>"

newtype ChannelName = ChannelName { getChannelName :: Text }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

data ChannelType
  = Secret
  | Private
  | Public
  deriving (Show, Eq)

data ChannelState = ChannelState
  { cstUsers :: Set Text
  , cstType :: ChannelType
  }
  deriving (Show, Eq, Generic)

data CoucouState
  = CoucouState
      { csLastUrl    :: Maybe Text
      , csCounter    :: Int
      , csYtAPIKey   :: YoutubeAPIKey
      , csChannels   :: Map ChannelName ChannelState
      , csSQLitePath :: FilePath
      }
  deriving (Show, Generic)

initialState :: YoutubeAPIKey -> FilePath -> CoucouState
initialState key fp = CoucouState
  { csLastUrl = Nothing
  , csCounter = 0
  , csChannels = mempty
  , csYtAPIKey = key
  , csSQLitePath = fp
  }
