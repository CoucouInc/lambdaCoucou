{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.ParserSpec where

import qualified Test.Hspec as H
import qualified Test.Hspec.Megaparsec as T.M
import qualified Text.Megaparsec as M

import qualified LambdaCoucou.Parser as LC.P
import qualified LambdaCoucou.Command as LC.Cmd

tests :: H.SpecWith ()
tests = H.describe "Parser" $ do

  H.describe "Command" $ do
    H.it "parses no op when no command" $
      LC.P.parseCommand "whatever" `T.M.shouldParse` LC.Cmd.Nop

    H.it "parses different prefixes" $ do
      LC.P.parseCommand "λurl" `T.M.shouldParse` LC.Cmd.Url Nothing
      LC.P.parseCommand "Σurl" `T.M.shouldParse` LC.Cmd.Url Nothing

    H.describe "url command" $ do
      H.it "parses url command" $
        LC.P.parseCommand "&url" `T.M.shouldParse` LC.Cmd.Url Nothing

      H.it "parses a target" $
        LC.P.parseCommand "&url > foo" `T.M.shouldParse` LC.Cmd.Url (Just "foo")

      H.it "parses a target with multiple space around >" $
        LC.P.parseCommand "&url  >   foo" `T.M.shouldParse` LC.Cmd.Url (Just "foo")

      H.it "doesn't parses a target when multi words after delimiter" $
        LC.P.parseCommand `T.M.shouldFailOn` "&url  >   foo bar"


  H.describe "Url" $ do
    let url = "https://foo.bar.com"
    let urlParser = M.parse LC.P.url ""

    H.it "fails on one word, not url" $
      urlParser `T.M.shouldFailOn` "foo"

    H.it "fails on only spaces" $
      urlParser `T.M.shouldFailOn` "   "

    H.it "succeed on a single http url" $ do
      let url' = "http://foo.bar.com"
      urlParser url' `T.M.shouldParse` url'

    H.it "succeed regardless of protocol case" $
      urlParser `T.M.shouldSucceedOn` "hTtP://foo.bar.com"

    H.it "succeed on a single https url" $
      urlParser url `T.M.shouldParse` url

    H.it "succeed on a url with some spaces before" $
      urlParser ("  " <> url) `T.M.shouldParse` url

    H.it "succeed on a url with a word before" $
      urlParser ("coucou  " <> url) `T.M.shouldParse` url

    H.it "succeed on a url with some spaces and a word before" $
      urlParser ("  coucou  " <> url) `T.M.shouldParse` url

    H.it "succeed on a url with many words before" $
      urlParser ("coucou charlie " <> url) `T.M.shouldParse` url

    H.it "succeed on a url with many words and spaces before" $
      urlParser ("  coucou charlie " <> url) `T.M.shouldParse` url

    H.it "suceed regardless of what is after the url" $
      urlParser (url <> " whatever") `T.M.shouldParse` url
