{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Help where

import Data.Monoid
import Data.Text


-- TODO find something more robust for help commands. This is too easy to fall out of sync
data CoucouHelpType
    = TypeCancer
    | TypeFactoid
    | TypeCoucou
    | TypeCoucouRank
    | TypeSeen
    | TypeTell
    | TypeRemind
    | TypeVersion
    | TypeRandom
    | TypeUrl
    | TypeCrypto
    | TypeCalendar
    | TypeUnknown !Text
    deriving (Eq, Show)

helpCommand :: Maybe CoucouHelpType -> Text
helpCommand Nothing = "List of commands: cancer, coucou, seen, tell, remind, version, factoid, random, url, crypto"
helpCommand (Just TypeCancer) =
    "cancer [search]: get a random cancer matching `search`. If no search is given, get any random cancer."
helpCommand (Just TypeCoucou) =
    "coucou [nick]: how many coucou for [nick]. If no nick is given, nick = sender of message."
helpCommand (Just TypeCoucouRank) = "List the best coucouteur"
helpCommand (Just TypeSeen) = "seen nick: when was the last time `nick` spoke ?"
helpCommand (Just TypeTell) =
    "tell nick [when] message: Next time `nick` speaks, tell her `message`. If a delay is given, wait at least this amount of time before delivering the message. Format of `when`: in [x hours] [y minutes] [z seconds]. Example: >tell Geekingfrog in 1 hour 2 minutes 10 seconds coucou"
helpCommand (Just TypeRemind) =
    "remind nick when message: Send `message` to `nick` in `when` time. Format of `when`: in [w weeks] [d days] [x hours] [y minutes] [z seconds]. Example: >remind me in 1 hour 2m 42 seconds coucou from the past."
helpCommand (Just TypeVersion) = "version: version of the bot."
helpCommand (Just TypeFactoid) =
    ">foo: get a random factoid named foo. >foo++ increment the foo counter by one. >foo := x reset factoid foo to value x. >foo += x adds x to the list of factoids for foo."
helpCommand (Just TypeRandom) = "get a random factoid"
helpCommand (Just (TypeUnknown term)) = "No command named " <> term <> "."
helpCommand (Just TypeUrl) = "Grab the title from the last url seen on the chan."
helpCommand (Just TypeCrypto) = "Exchange rate for Bitcoin (btc) and Ethereum (eth). Usage: crypto btc"
helpCommand (Just TypeCalendar) = "Current day according to the French Revolutionary calendar."
