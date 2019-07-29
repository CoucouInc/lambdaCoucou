{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.Cli where

import           Control.Monad.Identity (runIdentity)
import           Data.Generic.HKD       (HKD, build, construct)
import           Data.Text              (Text)
import           GHC.Generics
import qualified Options.Harg           as H

import qualified LambdaCoucou.State as LC.St

data Config
  = Config
      { chan :: Text
      , nick :: Text
      , ytApiKey :: LC.St.YoutubeAPIKey
      }
  deriving (Generic)

configOpt :: HKD Config H.Opt
configOpt = build @Config chanOpt nickOpt ytKeyOpt
  where
    chanOpt
      = H.optionWith H.strParser
        ( H.optLong "chan"
        . H.optShort 'c'
        . H.optHelp "Channel to connect to"
        . H.optDefault "#gougoutest"
        )

    nickOpt
      = H.optionWith H.strParser
        ( H.optLong "nick"
        . H.optShort 'n'
        . H.optHelp "Nickname of the bot"
        . H.optDefault "testLambdacoucou"
        )

    ytKeyOpt
      = H.optionWith H.strParser
        ( H.optLong "yt-key"
        . H.optShort 'y'
        . H.optHelp "Youtube API key to query metadata about videos"
        . H.optEnvVar "YT_API_KEY"
        )

getConfig :: IO Config
getConfig = do
  result <- H.execOpt H.EnvSource configOpt
  pure $ runIdentity (construct result)
