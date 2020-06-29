module LambdaCoucou.ParserSpec where

import qualified LambdaCoucou.Cancer as LC.Cancer
import qualified LambdaCoucou.Command as LC.Cmd
import qualified LambdaCoucou.Crypto as LC.C
import qualified LambdaCoucou.Help as LC.Hlp
import qualified LambdaCoucou.Parser as LC.P
import qualified LambdaCoucou.Remind as LC.R
import qualified LambdaCoucou.Url as LC.Url
import RIO
import qualified Test.Hspec as H
import qualified Test.Hspec.Megaparsec as T.M
import qualified Text.Megaparsec as M

tests :: H.SpecWith ()
tests = H.describe "Parser" $ do
  H.describe "Target" $ do
    let targetParser = M.parse LC.P.targetParser ""
    H.it "parses a target" $
      targetParser "> foo" `T.M.shouldParse` Just "foo"
    H.it "doesn't parse a non-target" $
      targetParser "foo bar" `T.M.shouldParse` Nothing
    H.it "parses a target with spaces around" $
      targetParser "  >  foo " `T.M.shouldParse` Just "foo"
    H.it "parses target with underscore" $
      targetParser "> foo_bar" `T.M.shouldParse` Just "foo_bar"

  H.describe "Command" $ do
    H.it "parses no op when no command" $
      LC.P.parseCommand "whatever" `T.M.shouldParse` LC.Cmd.Nop
    H.it "parses different prefixes" $ do
      LC.P.parseCommand "λurl" `T.M.shouldParse` LC.Cmd.Url 0 Nothing
      LC.P.parseCommand "Σurl" `T.M.shouldParse` LC.Cmd.Url 0 Nothing

    H.describe "url command" $ do
      H.it "parses url command" $
        LC.P.parseCommand "&url" `T.M.shouldParse` LC.Cmd.Url 0 Nothing
      H.it "parses url command with an offset" $
        LC.P.parseCommand "&url 4" `T.M.shouldParse` LC.Cmd.Url 4 Nothing
      H.it "parses url command with multi digit offset" $
        LC.P.parseCommand "&url 42" `T.M.shouldParse` LC.Cmd.Url 42 Nothing
      H.it "parses url command with multi digit offset and some trailing space" $
        LC.P.parseCommand "&url 42  " `T.M.shouldParse` LC.Cmd.Url 42 Nothing
      H.it "parses a target" $
        LC.P.parseCommand "&url > foo" `T.M.shouldParse` LC.Cmd.Url 0 (Just "foo")
      H.it "parses a target with an offset" $
        LC.P.parseCommand "&url 3 > foo" `T.M.shouldParse` LC.Cmd.Url 3 (Just "foo")
      H.it "parses a target with multiple space around >" $
        LC.P.parseCommand "&url  >   foo" `T.M.shouldParse` LC.Cmd.Url 0 (Just "foo")
      H.it "parses a target and an offset with multiple space around >" $
        LC.P.parseCommand "&url  2 >   foo" `T.M.shouldParse` LC.Cmd.Url 2 (Just "foo")
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
          `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Ethereum) (Just "foo")

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
          `T.M.shouldParse` LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo") Nothing
      H.it "parses random cancer with target" $
        LC.P.parseCommand "&cancer > foo"
          `T.M.shouldParse` LC.Cmd.Cancer LC.Cancer.RandomCancer (Just "foo")
      H.it "parses specific cancer with target" $
        LC.P.parseCommand "&cancer foo > bar"
          `T.M.shouldParse` LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo") (Just "bar")
      H.it "parses cancer with non alpha characters" $
        LC.P.parseCommand "&cancer foo.bar "
          `T.M.shouldParse` LC.Cmd.Cancer (LC.Cancer.SpecificCancer "foo.bar") Nothing

    H.describe "misc coucou command" $ do
      H.it "parses coucou command" $
        LC.P.parseCommand "&coucou  " `T.M.shouldParse` LC.Cmd.ShoutCoucou
      H.it "parses hey coucou command" $
        LC.P.parseCommand "&coucou foobared  " `T.M.shouldParse` LC.Cmd.HeyCoucou

    H.describe "help command" $ do
      H.it "parses general" $
        LC.P.parseCommand "&help" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.General Nothing
      H.it "parses url" $
        LC.P.parseCommand "&help url" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Url Nothing
      H.it "parses crypto" $
        LC.P.parseCommand "&help crypto" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Crypto Nothing
      H.it "parses date" $
        LC.P.parseCommand "&help date" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Date Nothing
      H.it "parses cancer" $
        LC.P.parseCommand "&help cancer" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Cancer Nothing
      H.it "parses coucou" $
        LC.P.parseCommand "&help coucou" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.ShoutCoucou Nothing
      H.it "parses joke" $
        LC.P.parseCommand "&help joke" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Joke Nothing
      H.it "parses unknown command" $
        LC.P.parseCommand "&help foobar"
          `T.M.shouldParse` LC.Cmd.Help (LC.Hlp.Unknown "foobar") Nothing
      H.it "parses general with a target" $
        LC.P.parseCommand "&help > foo" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.General (Just "foo")
      H.it "parses a command with a target" $
        LC.P.parseCommand "&help date > foo"
          `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Date (Just "foo")
      H.it "parses remind" $
        LC.P.parseCommand "&help remind" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Remind Nothing

    H.describe "pr command" $ do
      H.it "parses bare command" $
        LC.P.parseCommand "&pr" `T.M.shouldParse` LC.Cmd.PR Nothing
      H.it "parses command with target" $
        LC.P.parseCommand "&pr  > foo" `T.M.shouldParse` LC.Cmd.PR (Just "foo")

    H.describe "dadJoke command" $ do
      H.it "parses bare command" $
        LC.P.parseCommand "&joke" `T.M.shouldParse` LC.Cmd.Joke Nothing
      H.it "parses with target" $
        LC.P.parseCommand "&joke > foo" `T.M.shouldParse` LC.Cmd.Joke (Just "foo")

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
    H.it "suceed with some querystring" $
      urlParser (url <> "?query=1") `T.M.shouldParse` (url <> "?query=1")
    H.it "succeed with parens around the url" $
      urlParser (url <> ")") `T.M.shouldParse` url

  H.describe "Remind" $ do
    let ts =
          LC.R.TimeSpec
            { LC.R.dsYear = Just 2020,
              LC.R.dsMonth = Just 6,
              LC.R.dsDay = Just 15,
              LC.R.dsHour = Just 12,
              LC.R.dsMinute = Just 34
            }
    let tsNothing = LC.R.TimeSpec Nothing Nothing Nothing Nothing Nothing

    H.describe "Duration" $ do
      let parseTimeSpec = M.parse LC.P.durationParser "durationParser"
      H.it "parses general case" $
        parseTimeSpec "in 2020 y 6 months 15 day 12h 34min" `T.M.shouldParse` LC.R.RemindDuration ts
      H.it "parses missing elements" $
        parseTimeSpec "in 6 months 15 day 12h 34min"
          `T.M.shouldParse` LC.R.RemindDuration
            ts
              { LC.R.dsYear = Nothing
              }
      H.it "parses with only one element" $
        parseTimeSpec "in 20h"
          `T.M.shouldParse` LC.R.RemindDuration
            tsNothing
              { LC.R.dsHour = Just 20
              }
      H.it "fails when all fields are Nothing" $
        parseTimeSpec `T.M.shouldFailOn` ""

      H.it "handles minutes/months" $ do
        parseTimeSpec "in 1M"
          `T.M.shouldParse` LC.R.RemindDuration
            tsNothing
              { LC.R.dsMonth = Just 1
              }
        parseTimeSpec "in 2m"
          `T.M.shouldParse` LC.R.RemindDuration
            tsNothing
              { LC.R.dsMinute = Just 2
              }

      H.it "handles shorthand for hours and minutes" $ do
        parseTimeSpec "in 17h01"
          `T.M.shouldParse` LC.R.RemindDuration
            tsNothing
              { LC.R.dsHour = Just 17,
                LC.R.dsMinute = Just 1
              }
        parseTimeSpec "in 17h01m"
          `T.M.shouldParse` LC.R.RemindDuration
            tsNothing
              { LC.R.dsHour = Just 17,
                LC.R.dsMinute = Just 1
              }

    H.describe "command" $ do
      H.it "parse `at` with only date" $
        LC.P.parseCommand "&remind at 2020-06-15 text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.RemindTime $
                tsNothing
                  { LC.R.dsYear = Just 2020,
                    LC.R.dsMonth = Just 6,
                    LC.R.dsDay = Just 15
                  }
            )
            "text"

      H.it "parse `at` with only hours and minutes" $
        LC.P.parseCommand "&remind at 12:34 text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.RemindTime $
                tsNothing
                  { LC.R.dsHour = Just 12,
                    LC.R.dsMinute = Just 34
                  }
            )
            "text"

      H.it "parse full `at`" $
        LC.P.parseCommand "&remind at 2020-06-15 12:34 text"
          `T.M.shouldParse` LC.Cmd.Remind
            (LC.R.RemindTime ts)
            "text"

      H.it "parse `in`" $
        LC.P.parseCommand "&remind in 1 month 2d text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.RemindDuration $
                tsNothing
                  { LC.R.dsMonth = Just 1,
                    LC.R.dsDay = Just 2
                  }
            )
            "text"

      H.describe "tomorrow" $ do
        H.it "parses tomorrow with a time" $
          LC.P.parseCommand "&remind tomorrow at 12:34 text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.RemindTomorrow (Just (12, 34))
              )
              "text"

        H.it "parses just tomorrow" $
          LC.P.parseCommand "&remind tomorrow text"
            `T.M.shouldParse` LC.Cmd.Remind
              (LC.R.RemindTomorrow Nothing)
              "text"
