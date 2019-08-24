{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module LambdaCoucou.State where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.String
import qualified Data.Text       as Tx
import           GHC.Generics

newtype YoutubeAPIKey
  = YoutubeAPIKey { getYoutubeAPIKey :: Tx.Text }
  deriving Data.String.IsString via Tx.Text

instance Show YoutubeAPIKey where
  show _ = "YoutubeAPIKey <hidden>"

newtype ChannelName = ChannelName { getChannelName :: Tx.Text }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

data ChannelType
  = Secret
  | Private
  | Public
  deriving (Show, Eq)

data ChannelState = ChannelState
  { cstUsers :: Set.Set Tx.Text
  , cstType :: ChannelType
  }
  deriving (Show, Eq, Generic)

data CoucouState
  = CoucouState
      { csLastUrl    :: Maybe Tx.Text
      , csCounter    :: Int
      , csYtAPIKey   :: YoutubeAPIKey
      , csChannels   :: Map.Map ChannelName ChannelState
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
