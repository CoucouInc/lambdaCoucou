module LambdaCoucou.ParserSpec where

import qualified LambdaCoucou.Cancer as LC.Cancer
import qualified LambdaCoucou.Command as LC.Cmd
-- import qualified LambdaCoucou.Crypto as LC.C
import qualified LambdaCoucou.Help as LC.Hlp
import qualified LambdaCoucou.Parser as LC.P
import qualified LambdaCoucou.Remind as LC.R
-- import qualified LambdaCoucou.Url as LC.Url
import qualified LambdaCoucou.UserSettings as LC.Settings
import RIO
import qualified RIO.Time as Time
import qualified Test.Hspec as H
import qualified Test.Hspec.Megaparsec as T.M
import qualified Text.Megaparsec as M

tests :: H.SpecWith ()
tests = H.describe "Parser" $ do
  H.describe "Target" $ do
    let targetParser = M.parse LC.P.targetParser ""
    H.it "parses a target" $
      targetParser "> foo" `T.M.shouldParse` Just "foo"
    H.it "parses a target with spaces around" $
      targetParser "  >  foo " `T.M.shouldParse` Just "foo"
    H.it "parses target with underscore" $
      targetParser "> foo_bar" `T.M.shouldParse` Just "foo_bar"

  H.describe "Command" $ do
    H.it "parses no op when no command" $
      LC.P.parseCommand "whatever" `T.M.shouldParse` LC.Cmd.Nop
    H.it "parses different prefixes" $ do
      LC.P.parseCommand "λpr" `T.M.shouldParse` LC.Cmd.PR Nothing
      LC.P.parseCommand "Σpr" `T.M.shouldParse` LC.Cmd.PR Nothing

    -- H.describe "url command" $ do
    --   H.it "parses url command" $
    --     LC.P.parseCommand "&url" `T.M.shouldParse` LC.Cmd.Url 0 Nothing
    --   H.it "parses url command with an offset" $
    --     LC.P.parseCommand "&url 4" `T.M.shouldParse` LC.Cmd.Url 4 Nothing
    --   H.it "parses url command with multi digit offset" $
    --     LC.P.parseCommand "&url 42" `T.M.shouldParse` LC.Cmd.Url 42 Nothing
    --   H.it "parses url command with multi digit offset and some trailing space" $
    --     LC.P.parseCommand "&url 42  " `T.M.shouldParse` LC.Cmd.Url 42 Nothing
    --   H.it "parses a target" $
    --     LC.P.parseCommand "&url > foo" `T.M.shouldParse` LC.Cmd.Url 0 (Just "foo")
    --   H.it "parses a target with an offset" $
    --     LC.P.parseCommand "&url 3 > foo" `T.M.shouldParse` LC.Cmd.Url 3 (Just "foo")
    --   H.it "parses a target with multiple space around >" $
    --     LC.P.parseCommand "&url  >   foo" `T.M.shouldParse` LC.Cmd.Url 0 (Just "foo")
    --   H.it "parses a target and an offset with multiple space around >" $
    --     LC.P.parseCommand "&url  2 >   foo" `T.M.shouldParse` LC.Cmd.Url 2 (Just "foo")
    --   H.it "doesn't parses a target when multi words after delimiter" $
    --     LC.P.parseCommand `T.M.shouldFailOn` "&url  >   foo bar"

    -- H.describe "crypto command" $ do
    --   H.it "parses bitcoin" $
    --     LC.P.parseCommand "&crypto  btc" `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Bitcoin) Nothing
    --   H.it "xbt is the same as btc" $
    --     LC.P.parseCommand "&crypto  xbt" `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Bitcoin) Nothing
    --   H.it "parses ethereum" $
    --     LC.P.parseCommand "&crypto  eth" `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Ethereum) Nothing
    --   H.it "requires an argument" $
    --     LC.P.parseCommand `T.M.shouldFailOn` "&crypto  "
    --   H.it "only parses known coin" $
    --     LC.P.parseCommand "&crypto doge  " `T.M.shouldParse` LC.Cmd.Crypto (Left "doge") Nothing
    --   H.it "parses a target" $
    --     LC.P.parseCommand "&crypto  eth  >  foo "
    --       `T.M.shouldParse` LC.Cmd.Crypto (Right LC.C.Ethereum) (Just "foo")

    -- H.describe "date command" $ do
    --   H.it "parses date command" $
    --     LC.P.parseCommand "&date " `T.M.shouldParse` LC.Cmd.Date Nothing
    --   H.it "parses date with target" $
    --     LC.P.parseCommand "&date  > foo" `T.M.shouldParse` LC.Cmd.Date (Just "foo")

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
      -- H.it "parses url" $
      --   LC.P.parseCommand "&help url" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Url Nothing
      -- H.it "parses crypto" $
      --   LC.P.parseCommand "&help crypto" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Crypto Nothing
      -- H.it "parses date" $
      --   LC.P.parseCommand "&help date" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Date Nothing
      H.it "parses cancer" $
        LC.P.parseCommand "&help cancer" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Cancer Nothing
      H.it "parses coucou" $
        LC.P.parseCommand "&help coucou" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.ShoutCoucou Nothing
      -- H.it "parses joke" $
      --   LC.P.parseCommand "&help joke" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Joke Nothing
      H.it "parses unknown command" $
        LC.P.parseCommand "&help foobar"
          `T.M.shouldParse` LC.Cmd.Help (LC.Hlp.Unknown "foobar") Nothing
      H.it "parses general with a target" $
        LC.P.parseCommand "&help > foo" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.General (Just "foo")
      -- H.it "parses a command with a target" $
      --   LC.P.parseCommand "&help date > foo"
      --     `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Date (Just "foo")
      H.it "parses remind" $
        LC.P.parseCommand "&help remind" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Remind Nothing
      H.it "parses settings" $
        LC.P.parseCommand "&help settings" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.Settings Nothing
      H.it "parses ytSearch" $ do
        LC.P.parseCommand "&help yt_search" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.YTSearch Nothing
        LC.P.parseCommand "&help ytSearch" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.YTSearch Nothing
        LC.P.parseCommand "&help ytsearch" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.YTSearch Nothing
      H.it "parses liveStreams" $ do
        LC.P.parseCommand "&help live" `T.M.shouldParse` LC.Cmd.Help LC.Hlp.LiveStreams Nothing

    H.describe "pr command" $ do
      H.it "parses bare command" $
        LC.P.parseCommand "&pr" `T.M.shouldParse` LC.Cmd.PR Nothing
      H.it "parses command with target" $
        LC.P.parseCommand "&pr  > foo" `T.M.shouldParse` LC.Cmd.PR (Just "foo")

    -- H.describe "dadJoke command" $ do
    --   H.it "parses bare command" $
    --     LC.P.parseCommand "&joke" `T.M.shouldParse` LC.Cmd.Joke Nothing
    --   H.it "parses with target" $
    --     LC.P.parseCommand "&joke > foo" `T.M.shouldParse` LC.Cmd.Joke (Just "foo")

  -- H.describe "Url" $ do
  --   let url = "https://foo.bar.com"
  --   let urlParser = M.parse LC.Url.urlParser ""
  --   H.it "fails on one word, not url" $
  --     urlParser `T.M.shouldFailOn` "foo"
  --   H.it "fails on only spaces" $
  --     urlParser `T.M.shouldFailOn` "   "
  --   H.it "succeed on a single http url" $ do
  --     let url' = "http://foo.bar.com"
  --     urlParser url' `T.M.shouldParse` url'
  --   H.it "succeed regardless of protocol case" $
  --     urlParser `T.M.shouldSucceedOn` "hTtP://foo.bar.com"
  --   H.it "succeed on a single https url" $
  --     urlParser url `T.M.shouldParse` url
  --   H.it "succeed on a url with some spaces before" $
  --     urlParser ("  " <> url) `T.M.shouldParse` url
  --   H.it "succeed on a url with a word before" $
  --     urlParser ("coucou  " <> url) `T.M.shouldParse` url
  --   H.it "succeed on a url with some spaces and a word before" $
  --     urlParser ("  coucou  " <> url) `T.M.shouldParse` url
  --   H.it "succeed on a url with many words before" $
  --     urlParser ("coucou charlie " <> url) `T.M.shouldParse` url
  --   H.it "succeed on a url with words, punctuation and '>' before" $
  --     urlParser ("coucou: bla mo>o/  " <> url) `T.M.shouldParse` url
  --   H.it "succeed on a url with many words and spaces before" $
  --     urlParser ("  coucou charlie " <> url) `T.M.shouldParse` url
  --   H.it "suceed regardless of what is after the url" $
  --     urlParser (url <> " whatever") `T.M.shouldParse` url
  --   H.it "suceed with some querystring" $
  --     urlParser (url <> "?query=1") `T.M.shouldParse` (url <> "?query=1")
  --   H.it "succeed with parens around the url" $
  --     urlParser (url <> ")") `T.M.shouldParse` url

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
            ( LC.R.Reminder
                ( LC.R.RemindTime $
                    tsNothing
                      { LC.R.dsYear = Just 2020,
                        LC.R.dsMonth = Just 6,
                        LC.R.dsDay = Just 15
                      }
                )
                "text"
            )

      H.it "parse `at` with only hours and minutes with `:`" $
        LC.P.parseCommand "&remind at 12:34 text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.Reminder
                ( LC.R.RemindTime $
                    tsNothing
                      { LC.R.dsHour = Just 12,
                        LC.R.dsMinute = Just 34
                      }
                )
                "text"
            )

      H.it "parse `at` with only hours and minutes with `h`" $
        LC.P.parseCommand "&remind at 12h34 text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.Reminder
                ( LC.R.RemindTime $
                    tsNothing
                      { LC.R.dsHour = Just 12,
                        LC.R.dsMinute = Just 34
                      }
                )
                "text"
            )

      H.it "parse full `at`" $
        LC.P.parseCommand "&remind at 2020-06-15 12:34 text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.Reminder
                (LC.R.RemindTime ts)
                "text"
            )

      H.it "parse `in`" $
        LC.P.parseCommand "&remind in 1 month 2d text"
          `T.M.shouldParse` LC.Cmd.Remind
            ( LC.R.Reminder
                ( LC.R.RemindDuration $
                    tsNothing
                      { LC.R.dsMonth = Just 1,
                        LC.R.dsDay = Just 2
                      }
                )
                "text"
            )

      H.describe "tomorrow" $ do
        H.it "parses tomorrow with a time" $
          LC.P.parseCommand "&remind tomorrow at 12:34 text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  ( LC.R.RemindTomorrow (Just (12, 34))
                  )
                  "text"
              )

        H.it "parses tomorrow with only the hour" $
          LC.P.parseCommand "&remind tomorrow à 12 text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  ( LC.R.RemindTomorrow (Just (12, 0))
                  )
                  "text"
              )

        H.it "parses tomorrow with only the hour with a trailing h" $
          LC.P.parseCommand "&remind tomorrow à 12h text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  ( LC.R.RemindTomorrow (Just (12, 0))
                  )
                  "text"
              )

        H.it "parses just tomorrow" $
          LC.P.parseCommand "&remind tomorrow text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  (LC.R.RemindTomorrow Nothing)
                  "text"
              )

        H.it "parses just tomorrow in french" $
          LC.P.parseCommand "&remind demain text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  (LC.R.RemindTomorrow Nothing)
                  "text"
              )

      H.describe "weekdays" $ do
        H.it "parses reminder with just a weekday" $
          LC.P.parseCommand "&remind saturday text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  (LC.R.RemindWeekDay Time.Saturday Nothing)
                  "text"
              )

        H.it "parses reminder with just a weekday case insensitive and french" $
          LC.P.parseCommand "&remind MercRedi text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  (LC.R.RemindWeekDay Time.Wednesday Nothing)
                  "text"
              )

        H.it "parses reminder with a weekday and a time" $
          LC.P.parseCommand "&remind tuesday at 13:45 text"
            `T.M.shouldParse` LC.Cmd.Remind
              ( LC.R.Reminder
                  (LC.R.RemindWeekDay Time.Tuesday (Just (13, 45)))
                  "text"
              )

        H.it "parses remind list" $
          LC.P.parseCommand "&remind list  "
            `T.M.shouldParse` LC.Cmd.Remind LC.R.RemindList

        H.it "parses remind delete" $
          LC.P.parseCommand "&remind del 3 "
            `T.M.shouldParse` LC.Cmd.Remind (LC.R.RemindDelete 3)

  H.describe "User settings" $ do
    H.describe "timezone" $ do
      H.it "parses unset" $ do
        LC.P.parseCommand "&settings unset tz" `T.M.shouldParse` LC.Cmd.Settings (LC.Settings.UserTZ Nothing)
        LC.P.parseCommand "&settings unset tz" `T.M.shouldParse` LC.Cmd.Settings (LC.Settings.UserTZ Nothing)
        LC.P.parseCommand "&settings unset timezone" `T.M.shouldParse` LC.Cmd.Settings (LC.Settings.UserTZ Nothing)

      H.it "parses set timezone" $ do
        LC.P.parseCommand "&settings set tz whatever "
          `T.M.shouldParse` LC.Cmd.Settings (LC.Settings.UserTZ $ Just "whatever")

    H.describe "display" $ do
      H.it "can display settings" $
        LC.P.parseCommand "&settings show" `T.M.shouldParse` LC.Cmd.Settings LC.Settings.Display

  H.describe "Youtube search" $ do
    H.it "parses simple query" $
      LC.P.parseCommand "&yt_search query word"
        `T.M.shouldParse` LC.Cmd.YTSearch ["query", "word"] Nothing

    H.it
      "parses query with a target"
      $ LC.P.parseCommand "&yt_search query word > nick"
        `T.M.shouldParse` LC.Cmd.YTSearch ["query", "word"] (Just "nick")

    H.it
      "parses query with > and target"
      $ LC.P.parseCommand "&yt_search query > foo > nick"
        `T.M.shouldParse` LC.Cmd.YTSearch ["query", ">", "foo"] (Just "nick")

  H.describe "Sed" $ do
    H.it "can parse simple sed" $
      LC.P.parseCommand "s/foo/bar/" `T.M.shouldParse` LC.Cmd.Sed "foo" "bar"
    H.it "parses empty sed" $
      LC.P.parseCommand "s/foo//" `T.M.shouldParse` LC.Cmd.Sed "foo" ""
    H.it "must end with a slash" $
      LC.P.parseCommand "s/foo/bar" `T.M.shouldParse` LC.Cmd.Nop
    H.it "must have a search string" $
      LC.P.parseCommand "s//bar" `T.M.shouldParse` LC.Cmd.Nop

  H.describe "liveStreams" $ do
    H.it "can parse simple liveStreams command" $
      LC.P.parseCommand "λlive" `T.M.shouldParse` LC.Cmd.LiveStreams Nothing
    H.it "can parse liveStreams command with target" $
      LC.P.parseCommand "λlive > charlie" `T.M.shouldParse` LC.Cmd.LiveStreams (Just "charlie")

  H.describe "stupidcase" $ do
    H.it "can parse simple stupidcase command" $
      LC.P.parseCommand "&stupidcase coucou coucou coucoucou"
        `T.M.shouldParse` LC.Cmd.StupidCase ["coucou", "coucou", "coucoucou"] Nothing
    H.it "can parse simple stupid command" $
      LC.P.parseCommand "&stupid coucou coucou coucoucou"
        `T.M.shouldParse` LC.Cmd.StupidCase ["coucou", "coucou", "coucoucou"] Nothing
    H.it "can parse stupid command with target" $
      LC.P.parseCommand "&stupid coucou coucou coucoucou > charlie"
        `T.M.shouldParse` LC.Cmd.StupidCase ["coucou", "coucou", "coucoucou"] (Just "charlie")
