module LambdaCoucou.Parser where

import RIO

import qualified Text.Megaparsec          as M
import qualified Text.Megaparsec.Char     as C

import qualified LambdaCoucou.Cancer      as LC.Cancer
import qualified LambdaCoucou.Command     as LC.Cmd
import qualified LambdaCoucou.Crypto      as LC.C
import qualified LambdaCoucou.Help        as LC.Hlp
import qualified LambdaCoucou.ParserUtils as LC.P

type Parser = M.Parsec Void Text

parseCommand :: Text -> Either (M.ParseErrorBundle Text Void) LC.Cmd.CoucouCmd
parseCommand = M.parse commandParser "cmdParser"

commandParser :: Parser LC.Cmd.CoucouCmd
commandParser = prefix *> M.choice
  [ M.try urlCommandParser
  , M.try cryptoCommandParser
  , M.try dateCommandParser
  , M.try cancerCommandParser
  , M.try miscCoucouCommandParser
  , M.try helpCommandParser
  , M.try prCommandParser
  , M.try jokeCommandParser
  ]
  <|> pure LC.Cmd.Nop

prefix :: Parser Char
prefix = C.char 'λ' <|> C.char '&' <|> C.char 'Σ' -- Σ for sigma_g

-------------------- URL --------------------
urlCommandParser :: Parser LC.Cmd.CoucouCmd
urlCommandParser = do
  C.string "url"
  LC.Cmd.Url <$> targetParser


-------------------- Crypto --------------------
cryptoCommandParser :: Parser LC.Cmd.CoucouCmd
cryptoCommandParser = do
  C.string "crypto"
  LC.P.spaces
  LC.Cmd.Crypto <$> cryptoCoin <*> targetParser

cryptoCoin :: Parser (Either Text LC.C.CryptoCoin)
cryptoCoin
  = M.try (C.string' "btc" $> Right LC.C.Bitcoin)
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
  cancerType <- M.try
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
    [ M.try (f "crypto" LC.Hlp.Crypto)
    , M.try (f "date" LC.Hlp.Date)
    , M.try (f "cancer" LC.Hlp.Cancer)
    , M.try (f "coucou" LC.Hlp.ShoutCoucou)
    , M.try (f "joke" LC.Hlp.Joke)
    , M.try (LC.P.spaces *> (LC.Cmd.Help LC.Hlp.General <$> targetParser) <* M.eof)
    , LC.P.spaces *> (LC.Cmd.Help . LC.Hlp.Unknown <$> LC.P.utf8Word <*> targetParser)
    ]

  where
    f str cmd = LC.Cmd.Help <$> (M.some C.spaceChar *> (C.string str $> cmd)) <*> targetParser

-------------------- PR --------------------
prCommandParser :: Parser LC.Cmd.CoucouCmd
prCommandParser = do
  C.string "pr"
  LC.Cmd.PR <$> targetParser


-------------------- PR --------------------
jokeCommandParser :: Parser LC.Cmd.CoucouCmd
jokeCommandParser = do
  C.string "joke"
  LC.Cmd.Joke <$> targetParser


-------------------- Utils --------------------

targetParser :: Parser (Maybe Text)
targetParser = M.many C.spaceChar *> (
  ( C.char '>'
    *> M.some C.spaceChar
    *> fmap Just LC.P.utf8Word'
    <* LC.P.spaces
    <* M.eof
  )
  <|> pure Nothing
  )

