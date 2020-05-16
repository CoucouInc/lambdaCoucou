module Main where

import qualified LambdaCoucou.Bot as Bot
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 Bot.runBot
