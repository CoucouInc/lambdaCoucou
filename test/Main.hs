module Main where

import qualified Test.Hspec as H

import qualified LambdaCoucou.ParserSpec as Parser

main :: IO ()
main = H.hspec Parser.tests
