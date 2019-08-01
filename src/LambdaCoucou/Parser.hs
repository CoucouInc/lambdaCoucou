{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import           Control.Applicative      hiding (many, some)
import           Data.Functor             (($>))
import           Data.Text                (Text)
import           Data.Void                (Void)
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
  , M.try shoutCoucouCommandParser
  , M.try helpCommandParser
  , M.try prCommandParser
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
shoutCoucouCommandParser :: Parser LC.Cmd.CoucouCmd
shoutCoucouCommandParser = C.string "coucou" $> LC.Cmd.ShoutCoucou


-------------------- Help --------------------
helpCommandParser :: Parser LC.Cmd.CoucouCmd
helpCommandParser = do
  C.string "help"
  arg <- M.choice
    [ M.try (M.some C.spaceChar *> C.string "crypto" $> LC.Hlp.Crypto)
    , M.try (M.some C.spaceChar *> C.string "date" $> LC.Hlp.Date)
    , M.try (M.some C.spaceChar *> C.string "cancer" $> LC.Hlp.Cancer)
    , M.try (M.some C.spaceChar *> C.string "coucou" $> LC.Hlp.ShoutCoucou)
    , M.try (LC.P.spaces *> M.eof $> LC.Hlp.General)
    , LC.P.spaces *> (LC.Hlp.Unknown <$> LC.P.utf8Word)
    ]
  pure $ LC.Cmd.Help arg

-------------------- PR --------------------
prCommandParser :: Parser LC.Cmd.CoucouCmd
prCommandParser = do
  C.string "pr"
  LC.Cmd.PR <$> targetParser

-------------------- Utils --------------------

targetParser :: Parser (Maybe Text)
targetParser = M.many C.spaceChar *> (
  ( C.char '>'
    *> M.some C.spaceChar
    *> fmap Just LC.P.word
    <* LC.P.spaces
    <* M.eof
  )
  <|> pure Nothing
  )

