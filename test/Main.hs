module Main where

import qualified Test.Hspec as H

import qualified LambdaCoucou.ParserSpec as Parser
import qualified LambdaCoucou.UrlSpec    as Url

main :: IO ()
main = H.hspec $ do
  Parser.tests
  Url.tests
