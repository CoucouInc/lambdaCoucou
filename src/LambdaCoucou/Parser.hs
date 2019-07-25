{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import           Control.Applicative  hiding (many, some)
import           Control.Monad
import qualified Data.Char            as Chr
import           Data.Functor         (($>))
import           Data.Text            (Text)
import qualified Data.Text            as Tx
import           Data.Void            (Void)
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C

import qualified LambdaCoucou.Command as LC.Cmd
import qualified LambdaCoucou.Crypto  as LC.C
import qualified LambdaCoucou.Cancer  as LC.Cancer

type Parser = M.Parsec Void Text

hush :: Either e a -> Maybe a
hush = \case
  Left _ -> Nothing
  Right x -> Just x


parseCommand :: Text -> Either (M.ParseErrorBundle Text Void) LC.Cmd.CoucouCmd
parseCommand = M.parse commandParser "cmdParser"

commandParser :: Parser LC.Cmd.CoucouCmd
commandParser = prefix *> M.choice
  [ M.try urlCommandParser
  , M.try cryptoCommandParser
  , M.try dateCommandParser
  , M.try cancerCommandParser
  ]
  <|> pure LC.Cmd.Nop

prefix :: Parser Char
prefix = C.char 'λ' <|> C.char '&' <|> C.char 'Σ' -- Σ for sigma_g

-------------------- URL --------------------
urlCommandParser :: Parser LC.Cmd.CoucouCmd
urlCommandParser = do
  C.string "url"
  LC.Cmd.Url <$> targetParser


parseUrl :: Text -> Maybe Text
parseUrl = hush . M.parse url ""

url :: Parser Text
url
  = (M.eof *> fail "eof url")
  <|> (spaces *> (M.try url' <|> utf8Word *> url)) --  <|> (word *> spaces *> url))

url' :: Parser Text
url' = do
  proto <- C.string' "http://" <|> C.string' "https://"
  rest <- utf8Word'
  pure $ proto <> rest


-------------------- Crypto --------------------
cryptoCommandParser :: Parser LC.Cmd.CoucouCmd
cryptoCommandParser = do
  C.string "crypto"
  spaces
  LC.Cmd.Crypto <$> cryptoCoin <*> targetParser

cryptoCoin :: Parser (Either Text LC.C.CryptoCoin)
cryptoCoin
  = M.try (C.string' "btc" $> Right LC.C.Bitcoin)
  <|> M.try (C.string' "eth" $> Right LC.C.Ethereum)
  <|> (Left <$> word)


-------------------- Date --------------------
dateCommandParser :: Parser LC.Cmd.CoucouCmd
dateCommandParser = do
  C.string "date"
  spaces
  LC.Cmd.Date <$> targetParser


-------------------- Cancer --------------------
cancerCommandParser :: Parser LC.Cmd.CoucouCmd
cancerCommandParser = do
  C.string "cancer"
  cancerType <- M.try
    (M.some C.spaceChar *> (LC.Cancer.SpecificCancer <$> utf8Word'))
    <|> pure LC.Cancer.RandomCancer
  LC.Cmd.Cancer cancerType <$> targetParser


-------------------- Utils --------------------
word :: Parser Text
word = Tx.pack <$> M.some C.letterChar

-- parses a word with almost anything inside, except the specifier for a target '>'
utf8Word' :: Parser Text
utf8Word' = Tx.pack <$> M.some (M.satisfy (\c -> not (Chr.isSpace c) && c /= '>'))

utf8Word :: Parser Text
utf8Word = Tx.pack <$> M.some (M.satisfy (not . Chr.isSpace))

spaces :: Parser ()
spaces = void (M.many C.spaceChar)

targetParser :: Parser (Maybe Text)
targetParser = M.many C.spaceChar *> (
  ( C.char '>'
    *> M.some C.spaceChar
    *> fmap Just word
    <* spaces
    <* M.eof
  )
  <|> pure Nothing
  )
