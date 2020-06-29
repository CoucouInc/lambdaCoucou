module LambdaCoucou.Parser where

import qualified LambdaCoucou.Cancer as LC.Cancer
import qualified LambdaCoucou.Command as LC.Cmd
import qualified LambdaCoucou.Crypto as LC.C
import qualified LambdaCoucou.Help as LC.Hlp
import qualified LambdaCoucou.ParserUtils as LC.P
import qualified LambdaCoucou.Remind as LC.R
import RIO
import qualified RIO.Partial as RIO'
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

type Parser = M.Parsec Void Text

parseCommand :: Text -> Either (M.ParseErrorBundle Text Void) LC.Cmd.CoucouCmd
parseCommand = M.parse commandParser "cmdParser"

commandParser :: Parser LC.Cmd.CoucouCmd
commandParser =
  prefix
    *> M.choice
      [ M.try urlCommandParser,
        M.try cryptoCommandParser,
        M.try dateCommandParser,
        M.try cancerCommandParser,
        M.try miscCoucouCommandParser,
        M.try helpCommandParser,
        M.try prCommandParser,
        M.try jokeCommandParser,
        M.try remindCommandParser
      ]
    <|> pure LC.Cmd.Nop

prefix :: Parser Char
prefix = C.char 'λ' <|> C.char '&' <|> C.char 'Σ' -- Σ for sigma_g

-------------------- URL --------------------
urlCommandParser :: Parser LC.Cmd.CoucouCmd
urlCommandParser = do
  C.string "url"
  LC.Cmd.Url <$> (M.try offsetParser <|> pure 0) <*> targetParser
  where
    offsetParser = do
      M.some C.spaceChar
      d <- M.some C.digitChar
      pure $ RIO'.read d

-------------------- Crypto --------------------
cryptoCommandParser :: Parser LC.Cmd.CoucouCmd
cryptoCommandParser = do
  C.string "crypto"
  LC.P.spaces
  LC.Cmd.Crypto <$> cryptoCoin <*> targetParser

cryptoCoin :: Parser (Either Text LC.C.CryptoCoin)
cryptoCoin =
  M.try (C.string' "btc" $> Right LC.C.Bitcoin)
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
    [ M.try (f "url" LC.Hlp.Url),
      M.try (f "crypto" LC.Hlp.Crypto),
      M.try (f "date" LC.Hlp.Date),
      M.try (f "cancer" LC.Hlp.Cancer),
      M.try (f "coucou" LC.Hlp.ShoutCoucou),
      M.try (f "joke" LC.Hlp.Joke),
      M.try (f "remind" LC.Hlp.Remind),
      M.try (LC.P.spaces *> (LC.Cmd.Help LC.Hlp.General <$> targetParser) <* M.eof),
      LC.P.spaces *> (LC.Cmd.Help . LC.Hlp.Unknown <$> LC.P.utf8Word <*> targetParser)
    ]
  where
    f str cmd = LC.Cmd.Help <$> (M.some C.spaceChar *> (C.string str $> cmd)) <*> targetParser

-------------------- PR --------------------
prCommandParser :: Parser LC.Cmd.CoucouCmd
prCommandParser = do
  C.string "pr"
  LC.Cmd.PR <$> targetParser

-------------------- Joke --------------------
jokeCommandParser :: Parser LC.Cmd.CoucouCmd
jokeCommandParser = do
  C.string "joke"
  LC.Cmd.Joke <$> targetParser

-------------------- Remind --------------------
remindCommandParser :: Parser LC.Cmd.CoucouCmd
remindCommandParser = do
  C.string "remind"
  C.space1
  cmd <- timeAtParser <|> durationParser <|> tomorrowParser
  txt <- M.takeWhile1P (Just "text to remind") (const True)
  pure $ LC.Cmd.Remind cmd txt

timeAtParser :: Parser LC.R.RemindSpec
timeAtParser = do
  C.string "at"
  C.space1
  mbDate <- optional (M.try dateP)
  mbDateTime <- optional dateTimeP
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
  C.char ':'
  m <- LC.P.int
  C.space1 <|> M.eof
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
    minP = (void $ C.string "min" <* optional (C.char 's') <* end)
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
  time <- optional (C.string "at" *> C.space1 *> dateTimeP)
  pure $ LC.R.RemindTomorrow time

-------------------- Utils --------------------

targetParser :: Parser (Maybe Text)
targetParser =
  M.many C.spaceChar
    *> ( ( C.char '>'
             *> M.some C.spaceChar
             *> fmap Just LC.P.utf8Word'
             <* LC.P.spaces
             <* M.eof
         )
           <|> pure Nothing
       )
