{-# LANGUAGE OverloadedStrings #-}

module Main where

import LambdaCoucou.Run (run)

main :: IO ()
main = do
    let host = "chat.freenode.net"
    let port = 6697
    let nick = "lambdacoucou"
    run host port nick
