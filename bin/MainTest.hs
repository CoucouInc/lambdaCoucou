module Main where

import qualified LambdaCoucou.Twitch as Twitch
import qualified LambdaCoucou.TwitchHook as TwitchHook

main :: IO ()
main = Twitch.test -- TwitchHook.test
