{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import RIO
import qualified RIO.Text as T
import qualified RIO.Time as Time
import System.IO (putStrLn)
import qualified System.Environment as Env
import qualified UnliftIO.Async as Async
import UnliftIO.Concurrent (threadDelay)

import qualified Data.Aeson as JSON
import qualified LambdaCoucou.State as St
import qualified LambdaCoucou.Url as U
import qualified LambdaCoucou.Http as Http
import qualified LambdaCoucou.Twitch as Twitch
import qualified LambdaCoucou.Remind as Remind

main :: IO ()
main = do
  -- CM.test
  -- Twitch.test
  Remind.test
  putStrLn "all done"
