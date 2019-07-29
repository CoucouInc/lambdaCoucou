{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.ParserSpec where

import qualified Test.Hspec            as H
import qualified Test.Hspec.Megaparsec as T.M
import qualified Text.Megaparsec       as M

import qualified LambdaCoucou.Cancer   as LC.Cancer
import qualified LambdaCoucou.Command  as LC.Cmd
import qualified LambdaCoucou.Crypto   as LC.C
import qualified LambdaCoucou.Parser   as LC.P
import qualified LambdaCoucou.Url      as LC.Url

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

    H.describe "crypto command" $ do
      H.it "parses bitcoin" $
        LC.P.parseCommand "&crypto  btc" `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Bitcoin) Nothing

      H.it "parses ethereum" $
        LC.P.parseCommand "&crypto  eth" `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Ethereum) Nothing

      H.it "requires an argument" $
        LC.P.parseCommand `T.M.shouldFailOn` "&crypto  "

      H.it "only parses known coin" $
        LC.P.parseCommand "&crypto doge  " `T.M.shouldParse` LC.Cmd.Crypto (Left "doge") Nothing

      H.it "parses a target" $
        LC.P.parseCommand "&crypto  eth  >  foo "
        `T.M.shouldParse`
        LC.Cmd.Crypto (Right LC.C.Ethereum) (Just "foo")

    H.describe "date command" $ do
      H.it "parses date command" $
        LC.P.parseCommand "&date " `T.M.shouldParse` LC.Cmd.Date Nothing

      H.it "parses date with target" $
        LC.P.parseCommand "&date  > foo" `T.M.shouldParse` LC.Cmd.Date (Just "foo")

    H.describe "cancer command" $ do
      H.it "parses random cancer" $
        LC.P.parseCommand "&cancer " `T.M.shouldParse` LC.Cmd.Cancer LC.Cancer.RandomCancer Nothing

      H.it "parses specific cancer" $
        LC.P.parseCommand "&cancer foo "
        `T.M.shouldParse`
        LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo") Nothing

      H.it "parses random cancer with target" $
        LC.P.parseCommand "&cancer > foo"
        `T.M.shouldParse`
        LC.Cmd.Cancer LC.Cancer.RandomCancer (Just "foo")

      H.it "parses specific cancer with target" $
        LC.P.parseCommand "&cancer foo > bar"
        `T.M.shouldParse`
        LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo") (Just "bar")

      H.it "parses cancer with non alpha characters" $
        LC.P.parseCommand "&cancer foo.bar "
        `T.M.shouldParse`
        LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo.bar") Nothing

    H.describe "shout coucou command" $
      H.it "parses coucou command" $
        LC.P.parseCommand "&coucou" `T.M.shouldParse` LC.Cmd.ShoutCoucou


  H.describe "Url" $ do
    let url = "https://foo.bar.com"
    let urlParser = M.parse LC.Url.urlParser ""

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

    H.it "succeed on a url with words, punctuation and '>' before" $
      urlParser ("coucou: bla mo>o/  " <> url) `T.M.shouldParse` url

    H.it "succeed on a url with many words and spaces before" $
      urlParser ("  coucou charlie " <> url) `T.M.shouldParse` url

    H.it "suceed regardless of what is after the url" $
      urlParser (url <> " whatever") `T.M.shouldParse` url
