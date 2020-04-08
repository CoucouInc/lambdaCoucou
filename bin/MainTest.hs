{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import RIO

import qualified LambdaCoucou.Twitch as Twitch
import qualified LambdaCoucou.TwitchHook as TwitchHook

main :: IO ()
main = Twitch.test -- TwitchHook.test
