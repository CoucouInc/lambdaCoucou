{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module LambdaCoucou.State where

import Control.Concurrent.STM.TVar
import qualified Data.Text as Tx
import GHC.Generics

newtype YoutubeAPIKey
  = YoutubeAPIKey { getYoutubeAPIKey :: Tx.Text }

instance Show YoutubeAPIKey where
  show _ = "YoutubeAPIKey <hidden>"

data CoucouState
  = CoucouState
      { csLastUrl :: Maybe Tx.Text
      , csCounter :: Int
      , csYtAPIKey :: YoutubeAPIKey
      }
  deriving (Show, Generic)

initialState :: YoutubeAPIKey -> CoucouState
initialState key = CoucouState
  { csLastUrl = Nothing
  , csCounter = 0
  , csYtAPIKey = key
  }
