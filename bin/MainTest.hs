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

import qualified Data.Aeson as JSON
import qualified LambdaCoucou.State as St
import qualified LambdaCoucou.Url as U
import qualified LambdaCoucou.Http as Http

main :: IO ()
main = do
  -- CM.test
  now <- Time.getCurrentTime
  let d = Time.utctDay now
  putStrLn $ show $ Time.addGregorianMonthsClip (-2) d
  putStrLn $ show $ Time.addDays (-61) d
  putStrLn "all done"
