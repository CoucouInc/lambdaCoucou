{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import RIO
import qualified RIO.Text as T
import System.IO (putStrLn)
import qualified System.Environment as Env

import qualified Data.Aeson as JSON
import qualified LambdaCoucou.State as St
import qualified LambdaCoucou.Url as U
import qualified LambdaCoucou.Http as Http

main :: IO ()
main = do
  Just key <- Env.lookupEnv "YT_API_KEY"
  resp <- U.runFetch $ U.fetchYtUrlData
    (St.YoutubeAPIKey $ T.pack key)
    "https://www.youtube.com/watch?v=QVzfg61uTwE"

  putStrLn $ show resp

  putStrLn "all done"
