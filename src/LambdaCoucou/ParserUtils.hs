module LambdaCoucou.ParserUtils where

import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C
import Data.Void
import           Data.Text            (Text)
import qualified Data.Text            as Tx

import qualified Data.Char            as Chr
import           Control.Monad

type Parser = M.Parsec Void Text

word :: Parser Text
word = Tx.pack <$> M.some C.letterChar

-- parses a word with almost anything inside, except the specifier for a target '>'
utf8Word' :: Parser Text
utf8Word' = Tx.pack <$> M.some (M.satisfy (\c -> not (Chr.isSpace c) && c /= '>'))

utf8Word :: Parser Text
utf8Word = Tx.pack <$> M.some (M.satisfy (not . Chr.isSpace))

spaces :: Parser ()
spaces = void (M.many C.spaceChar)
