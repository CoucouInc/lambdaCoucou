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

import qualified LambdaCoucou.Crypto as Crypto

main :: IO ()
main = do
  resp <- Crypto.getRateInEuro Crypto.Bitcoin
  putStrLn $ show resp
  putStrLn "all done"
