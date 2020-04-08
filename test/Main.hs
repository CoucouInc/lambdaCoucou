module Main where

import qualified LambdaCoucou.ParserSpec as Parser
import qualified LambdaCoucou.UrlSpec as Url
import RIO
import qualified Test.Hspec as H

main :: IO ()
main = H.hspec $ do
  Parser.tests
  Url.tests
