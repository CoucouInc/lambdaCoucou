{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import RIO
import System.IO (putStrLn)
import qualified System.Environment as Env

import qualified LambdaCoucou.Twitch as Twitch
import qualified LambdaCoucou.TwitchHook as TwitchHook

import qualified Data.RingBuffer as RB

main :: IO ()
-- main = Twitch.test -- TwitchHook.test
main = do
  rb <- RB.new @Vector 5
  RB.toList rb >>= putStrLn . show
  traverse_ (\x -> RB.append x rb) [(0::Int)..10]
  RB.latest rb 0 >>= putStrLn . show
  RB.latest rb 1 >>= putStrLn . show
  RB.latest rb 1000 >>= putStrLn . show

  Env.lookupEnv "FOO" >>= putStrLn . show
