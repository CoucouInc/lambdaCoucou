module LambdaCoucou.Parser where

import qualified LambdaCoucou.Cancer as LC.Cancer
import qualified LambdaCoucou.Command as LC.Cmd
import qualified LambdaCoucou.Crypto as LC.C
import qualified LambdaCoucou.Help as LC.Hlp
import qualified LambdaCoucou.ParserUtils as LC.P
import qualified LambdaCoucou.Remind as LC.R
import qualified LambdaCoucou.UserSettings as LC.Settings
import RIO
import qualified RIO.Text as T
-- import qualified RIO.Partial as RIO'
import qualified RIO.Time as Time
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import Control.Applicative.Combinators (manyTill_, manyTill, someTill)

type Parser = M.Parsec Void Text

parseCommand :: Text -> Either (M.ParseErrorBundle Text Void) LC.Cmd.CoucouCmd
parseCommand = M.parse commandParser "cmdParser"

commandParser :: Parser LC.Cmd.CoucouCmd
commandParser =
  M.try sedCommandParser
  <|> prefix
    *> M.choice
      -- [ M.try urlCommandParser,
        -- M.try cryptoCommandParser,
        -- M.try dateCommandParser,
      [ M.try cancerCommandParser,
        M.try miscCoucouCommandParser,
        M.try helpCommandParser,
        M.try prCommandParser,
        -- M.try jokeCommandParser,
        M.try remindCommandParser,
        M.try settingsCommandParser,
        M.try ytSearchCommandParser,
        M.try liveStreamsCommandParser,
        M.try stupidCaseCommandParser
      ]
    <|> pure LC.Cmd.Nop

prefix :: Parser Char
prefix = C.char 'λ' <|> C.char '&' <|> C.char 'Σ' -- Σ for sigma_g

-- -------------------- URL --------------------
-- urlCommandParser :: Parser LC.Cmd.CoucouCmd
-- urlCommandParser = do
--   C.string "url"
--   LC.Cmd.Url <$> (M.try offsetParser <|> pure 0) <*> targetParser
--   where
--     offsetParser = do
--       M.some C.spaceChar
--       d <- M.some C.digitChar
--       pure $ RIO'.read d

-------------------- Crypto --------------------
cryptoCommandParser :: Parser LC.Cmd.CoucouCmd
cryptoCommandParser = do
  C.string "crypto"
  LC.P.spaces
  LC.Cmd.Crypto <$> cryptoCoin <*> targetParser

cryptoCoin :: Parser (Either Text LC.C.CryptoCoin)
cryptoCoin =
  M.try ((C.string' "btc" <|> C.string' "xbt") $> Right LC.C.Bitcoin)
    <|> M.try (C.string' "eth" $> Right LC.C.Ethereum)
    <|> (Left <$> LC.P.word)

-------------------- Date --------------------
dateCommandParser :: Parser LC.Cmd.CoucouCmd
dateCommandParser = do
  C.string "date"
  LC.P.spaces
  LC.Cmd.Date <$> targetParser

-------------------- Cancer --------------------
cancerCommandParser :: Parser LC.Cmd.CoucouCmd
cancerCommandParser = do
  C.string "cancer"
  cancerType <-
    M.try
      (M.some C.spaceChar *> (LC.Cancer.SpecificCancer <$> LC.P.utf8Word'))
      <|> pure LC.Cancer.RandomCancer
  LC.Cmd.Cancer cancerType <$> targetParser

-------------------- Coucou --------------------
miscCoucouCommandParser :: Parser LC.Cmd.CoucouCmd
miscCoucouCommandParser = do
  C.string "coucou"
  LC.P.spaces
  (M.eof $> LC.Cmd.ShoutCoucou) <|> ((LC.P.utf8Word <* LC.P.spaces <* M.eof) $> LC.Cmd.HeyCoucou)

-------------------- Help --------------------
helpCommandParser :: Parser LC.Cmd.CoucouCmd
helpCommandParser = do
  C.string "help"
  M.choice
    -- [ M.try (f "url" LC.Hlp.Url),
      -- M.try (f "crypto" LC.Hlp.Crypto),
      -- M.try (f "date" LC.Hlp.Date),
    [ M.try (f "cancer" LC.Hlp.Cancer),
      M.try (f "coucou" LC.Hlp.ShoutCoucou),
      -- M.try (f "joke" LC.Hlp.Joke),
      M.try (f "remind" LC.Hlp.Remind),
      M.try (f "settings" LC.Hlp.Settings),
      M.try (f "ytSearch" LC.Hlp.YTSearch),
      M.try (f "yt_search" LC.Hlp.YTSearch),
      M.try (f "live" LC.Hlp.LiveStreams),
      M.try (f "stupidcase" LC.Hlp.StupidCase),
      M.try (f "stupid" LC.Hlp.StupidCase),
      M.try (LC.P.spaces *> (LC.Cmd.Help LC.Hlp.General <$> targetParser) <* M.eof),
      LC.P.spaces *> (LC.Cmd.Help . LC.Hlp.Unknown <$> LC.P.utf8Word <*> targetParser)
    ]
  where
    f str cmd = LC.Cmd.Help <$> (M.some C.spaceChar *> (C.string' str $> cmd)) <*> targetParser

-------------------- PR --------------------
prCommandParser :: Parser LC.Cmd.CoucouCmd
prCommandParser = do
  C.string "pr"
  LC.Cmd.PR <$> targetParser

-- -------------------- Joke --------------------
-- jokeCommandParser :: Parser LC.Cmd.CoucouCmd
-- jokeCommandParser = do
--   C.string "joke"
--   LC.Cmd.Joke <$> targetParser

-------------------- Remind --------------------
remindCommandParser :: Parser LC.Cmd.CoucouCmd
remindCommandParser = do
  C.string "remind"
  C.space1
  LC.Cmd.Remind <$> (M.try reminder <|> M.try remindList <|> remindDelete)

reminder :: Parser LC.R.RemindCmd
reminder = do
  remindSpec <- timeAtParser <|> durationParser <|> tomorrowParser <|> weekdayParser
  txt <- M.takeWhile1P (Just "text to remind") (const True)
  pure $ LC.R.Reminder remindSpec txt

remindList :: Parser LC.R.RemindCmd
remindList = C.string "list" $> LC.R.RemindList

remindDelete :: Parser LC.R.RemindCmd
remindDelete = do
  C.string "del"
  C.space1
  reminderId <- LC.P.int
  pure $ LC.R.RemindDelete reminderId

timeAtParser :: Parser LC.R.RemindSpec
timeAtParser = do
  C.string "at"
  C.space1
  mbDate <- optional (M.try dateP)
  mbDateTime <- optional (dateTimeP <* C.space1)
  if isNothing mbDate && isNothing mbDateTime
    then fail "all fields are Nothing"
    else
      let (y, m, d) = case mbDate of
            Nothing -> (Nothing, Nothing, Nothing)
            Just (a, b, c) -> (Just a, Just b, Just c)
          (h, min) = case mbDateTime of
            Nothing -> (Nothing, Nothing)
            Just (a, b) -> (Just a, Just b)
          ts = LC.R.TimeSpec y m d h min
       in pure $ LC.R.RemindTime ts
  where
    dateP = do
      y <- LC.P.int
      C.char '-'
      m <- LC.P.int
      C.char '-'
      d <- LC.P.int
      void (C.char 'T') <|> void (C.char ' ') <|> M.eof
      pure (y, m, d)

dateTimeP :: Parser (Int, Int)
dateTimeP = do
  h <- LC.P.int
  void $ optional (C.char 'h' <|> C.char ':')
  m <- fromMaybe 0 <$> optional LC.P.int
  pure (h, m)

durationParser :: Parser LC.R.RemindSpec
durationParser = do
  C.string "in"
  C.space1
  y <- optional (M.try $ LC.P.int <* LC.P.spaces <* yearP)
  m <- optional (M.try $ LC.P.int <* LC.P.spaces <* monthP)
  d <- optional (M.try $ LC.P.int <* LC.P.spaces <* dayP)
  (h, minutes) <-
    ( M.try hourMinP <|> do
        h <- optional (M.try $ LC.P.int <* LC.P.spaces <* hourP)
        minutes <- optional (M.try $ LC.P.int <* LC.P.spaces <* minP)
        pure (h, minutes)
      )
  case (y <|> m <|> d <|> h <|> minutes) of
    Nothing -> fail "all fields are Nothing"
    Just _ ->
      pure $
        LC.R.RemindDuration $
          LC.R.TimeSpec
            { LC.R.dsYear = y,
              LC.R.dsMonth = m,
              LC.R.dsDay = d,
              LC.R.dsHour = h,
              LC.R.dsMinute = minutes
            }
  where
    yearP =
      (void $ C.string "year" <* optional (C.char 's') <* end)
        <|> (M.try (void $ C.char 'y') <* end)
    monthP =
      (void $ C.string "month" <* optional (C.char 's') <* end)
        <|> (M.try (void $ C.char 'M') <* end)
    dayP =
      (void $ C.string "day" <* optional (C.char 's') <* end)
        <|> (M.try (void $ C.char 'd') <* end)
    hourP =
      (void $ C.string "hour" <* optional (C.char 's') <* end)
        <|> (M.try (void $ C.char 'h') <* end)
    minP =
      (void $ C.string "min" <* optional (C.char 's') <* end)
        <|> (M.try (void $ C.char 'm') <* end)
    hourMinP = do
      h <- (LC.P.int <* (C.char 'h' <|> C.char 'H'))
      m <- LC.P.int
      optional (C.char 'm')
      end
      pure (Just h, Just m)
    -- the eof shouldn't parse anything in a real message but is handy for testing
    end = M.eof <|> C.space1

tomorrowParser :: Parser LC.R.RemindSpec
tomorrowParser = do
  C.string "tomorrow" <|> C.string "demain"
  C.space1 <|> M.eof
  time <- optional (atTime *> C.space1 *> dateTimeP <* C.space1)
  pure $ LC.R.RemindTomorrow time

weekdayParser :: Parser LC.R.RemindSpec
weekdayParser = do
  d <- weekdayP
  C.space1
  time <- optional (atTime *> C.space1 *> dateTimeP <* C.space1)
  pure $ LC.R.RemindWeekDay d time
  where
    weekdayP =
      M.choice
        [ (C.string' "monday" <|> C.string' "lundi") $> Time.Monday,
          (C.string' "tuesday" <|> C.string' "mardi") $> Time.Tuesday,
          (C.string' "wednesday" <|> C.string' "mercredi") $> Time.Wednesday,
          (C.string' "thursday" <|> C.string' "jeudi") $> Time.Thursday,
          (C.string' "friday" <|> C.string' "vendredi") $> Time.Friday,
          (C.string' "saturday" <|> C.string' "samedi") $> Time.Saturday,
          (C.string' "sunday" <|> C.string' "dimanche") $> Time.Sunday
        ]

atTime :: Parser Text
atTime = C.string "at" <|> C.string "à"

-------------------- User Settings --------------------
settingsCommandParser :: Parser LC.Cmd.CoucouCmd
settingsCommandParser = do
  C.string "settings"
  C.space1
  cmd <- displayP <|> tzP
  pure $ LC.Cmd.Settings cmd

  where
    displayP = C.string "show" $> LC.Settings.Display
    tzP = do
      isSet <- (C.string "set" $> True <|> C.string "unset" $> False)
      C.space1
      (C.string "tz" <|> C.string "timezone")
      LC.Settings.UserTZ <$> if isSet
        then Just <$> (C.space1 *> LC.P.utf8Word')
        else pure Nothing

-------------------- Youtube Search --------------------
ytSearchCommandParser :: Parser LC.Cmd.CoucouCmd
ytSearchCommandParser = do
  C.string "yt_search" <|> C.string' "ytSearch"
  C.space1
  (queryWords, target) <- (LC.P.utf8Word <* C.space) `manyTill_` targetParser
  pure $ LC.Cmd.YTSearch queryWords target


-------------------- Sed --------------------
-- note that this parser only handle s/foo/bar(/?)
-- and ignores escaping
sedCommandParser :: Parser LC.Cmd.CoucouCmd
sedCommandParser = do
  C.string "s/"
  rawRegex <- T.pack <$> M.anySingle `someTill` C.char '/'
  replacement <- T.pack <$> M.anySingle `manyTill` (C.char '/' *> LC.P.spaces *> M.eof)
  pure $ LC.Cmd.Sed rawRegex replacement

-------------------- Lives --------------------
liveStreamsCommandParser :: Parser LC.Cmd.CoucouCmd
liveStreamsCommandParser = do
  C.string "live"
  LC.P.spaces
  LC.Cmd.LiveStreams <$> targetParser

------------------- StUpIdCaSe ----------------
stupidCaseCommandParser :: Parser LC.Cmd.CoucouCmd
stupidCaseCommandParser = do
  C.string "stupidcase" <|> C.string "stupid"
  C.space1
  (words, target) <- (LC.P.utf8Word <* C.space) `manyTill_` targetParser
  pure $ LC.Cmd.StupidCase words target

-------------------- Utils --------------------

targetParser :: Parser (Maybe Text)
targetParser = C.space *> (M.try t <|> (M.eof $> Nothing))
  where
    t = do
      C.char '>'
      C.space1
      target <- LC.P.utf8Word'
      C.space
      M.eof
      pure (Just target)
