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
  <|> (spaces *> (M.try url' <|> word *> url)) --  <|> (word *> spaces *> url))

url' :: Parser Text
url' = do
  proto <- C.string' "http://" <|> C.string' "https://"
  rest <- M.some (M.satisfy (not . Chr.isSpace))
  pure $ proto <> Tx.pack rest


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


-------------------- Utils --------------------
word :: Parser Text
word = Tx.pack <$> M.some C.letterChar

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
