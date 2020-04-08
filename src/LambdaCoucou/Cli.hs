{-# LANGUAGE StrictData #-}

module LambdaCoucou.Cli where

import RIO

import           Control.Monad.Identity (runIdentity)
import           Data.Generic.HKD       (HKD, build, construct)
import qualified Options.Harg           as H

import qualified LambdaCoucou.State as LC.St

data Config
  = Config
      { chan       :: Text
      , nick       :: Text
      , ytApiKey   :: LC.St.YoutubeAPIKey
      , sqlitePath :: FilePath
      }
  deriving (Generic)

configOpt :: HKD Config H.Opt
configOpt = build @Config chanOpt nickOpt ytKeyOpt sqlitePathOpts
  where
    chanOpt
      = H.option H.strParser
        ( H.long "chan"
        . H.short 'c'
        . H.help "Channel to connect to"
        . H.defaultVal "#gougoutest"
        )

    nickOpt
      = H.option H.strParser
        ( H.long "nick"
        . H.short 'n'
        . H.help "Nickname of the bot"
        . H.defaultVal "testLambdacoucou"
        )

    ytKeyOpt
      = H.option H.strParser
        ( H.long "yt-key"
        . H.short 'y'
        . H.help "Youtube API key to query metadata about videos"
        . H.envVar "YT_API_KEY"
        )

    sqlitePathOpts
      = H.option H.strParser
        ( H.long "sqlite-path"
        . H.help "Filepath to the sqlite db"
        . H.envVar "SQLITE_FILEPATH"
        )

getConfig :: IO Config
getConfig = do
  result <- H.execOpt H.EnvSource configOpt
  pure $ runIdentity (construct result)
