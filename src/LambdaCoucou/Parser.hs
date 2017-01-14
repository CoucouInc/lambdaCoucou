{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Data.Monoid ((<>))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import LambdaCoucou.Types (CoucouCmd(..), CmdFactoidType(..), Timestamp)

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand raw = parse commandParser "" raw

-- if it doesn't start with the special character, ignore the command
commandParser :: Parser CoucouCmd
commandParser = try (prefix *> commandParser' <* space) <|> try incCoucou <|> pure CoucouCmdNop

prefix :: Parser Char
prefix = char 'λ' <|> char '>'

commandParser' :: Parser CoucouCmd
commandParser' =
    try getVersion <|> try tell <|> try getCoucou <|> try see <|> try search <|> try lastSeen <|>
    try cancer <|>
    factoid

cancer :: Parser CoucouCmd
cancer = do
    string "cancer"
    try randomCancer <|> (some spaceChar >> (try searchCancer <|> try hlCancer <|> searchHlCancer))
    where
        randomCancer = return (CoucouCmdCancer Nothing Nothing) <* end
        searchCancer = do -- searchParser >>= \s -> return $ CoucouCmdCancer (Just s) Nothing <* end
            s <- searchParser
            end
            return $ CoucouCmdCancer (Just s) Nothing
        hlCancer = CoucouCmdCancer Nothing <$> maybeHl <* end
        searchHlCancer = do
            s <- searchParser
            some spaceChar
            hl <- maybeHl
            end
            return $ CoucouCmdCancer (Just s) hl
        searchParser = T.pack <$> some (satisfy (not . isSpace))


parseCancer :: Text -> Either (ParseError Char Dec) [(Text, Text)]
parseCancer raw = parse cancerParser "" raw

cancerParser :: Parsec Dec Text [(Text, Text)]
cancerParser = many cancerLine

cancerLine :: Parsec Dec Text (Text, Text)
cancerLine = do
    title <- T.pack <$> many (noneOf [':'])
    string ": "
    url <- T.pack <$> manyTill anyChar eol
    return (title, url)

factoid :: Parser CoucouCmd
factoid = try counterFactoid <|> try setFactoid <|> getFactoid

counterFactoid :: Parser CoucouCmd
counterFactoid = try counterPlus <|> counterMinus

counterPlus :: Parser CoucouCmd
counterPlus = do
    name <- genericCounter (string "++") >>= validFactoidName
    return (CoucouCmdFactoid name IncFactoid)

counterMinus :: Parser CoucouCmd
counterMinus = do
    name <- genericCounter (string "--") >>= validFactoidName
    return (CoucouCmdFactoid name DecFactoid)

genericCounter :: Parser String -> Parser Text
genericCounter limit = T.pack <$> (someTill (satisfy (not . isSpace)) limit <* space <* eof)

setFactoid :: Parser CoucouCmd
setFactoid = do
    let operatorParser = string "=" <|> string ":=" <|> string "+="
    name <- T.pack <$> someTill (satisfy (not . isSpace)) (lookAhead (some spaceChar <|> operatorParser))
    space
    operator <- operatorParser
    space
    val <- T.pack <$> many anyChar
    eof
    if T.null val
        then return $ CoucouCmdFactoid name DeleteFactoid
        else do
            let op
                    | operator == "=" = SetFactoid
                    | operator == ":=" = ResetFactoid
                    | otherwise = AugmentFactoid
            return $ CoucouCmdFactoid name (op val)

validFactoidName :: Text -> Parser Text
validFactoidName name =
    if name `elem` factoidBlackList
        then fail $ T.unpack $ "Reserved keyword " <> name
        else return name

-- reserved name, these cannot be used for factoids
factoidBlackList :: [Text]
factoidBlackList = ["see", "search", "coucou", "seen", "cancer", "version"]

factoidName :: Maybe (Parser String) -> Parser Text
factoidName mbLimit = do
    let limit = fromMaybe (many spaceChar) mbLimit
    name <- T.pack <$> someTill (satisfy (not . isSpace)) (try $ lookAhead (some spaceChar <|> limit))
    if name `elem` factoidBlackList
    then fail $ T.unpack $ "Reserved keyword " <> name
    else return name

word :: Parser Text
-- word = T.pack <$> someTill (satisfy (not . isSpace)) (void spaceChar <|> eof)
word = T.pack <$> someTill (satisfy (not . isSpace)) (void spaceChar <|> eof)

getFactoid :: Parser CoucouCmd
getFactoid = do
    name <- word >>= validFactoidName
    space
    mbHl <- maybeHl
    end
    return $ CoucouCmdFactoid name (GetFactoid mbHl)


getCoucou :: Parser CoucouCmd
getCoucou = do
    string "coucou"
    target <-
        try (many spaceChar *> eof *> return Nothing) -- (only space afterwards)
         <|>
        (some spaceChar >> (Just <$> nick)) -- (at least one space then a nick)
    return $ CoucouCmdGetCoucou target

incCoucou :: Parser CoucouCmd
incCoucou = do
    space
    try (string "coucou" <* (eof <|> void spaceChar)) <|>
        manyTill anyChar (string " coucou" <* (eof <|> void (some spaceChar)))
    return CoucouCmdIncCoucou

nick :: Parser Text
nick = T.pack <$> some (satisfy (not . isSpace))

maybeHl :: Parser (Maybe Text)
maybeHl = optional (char '>' >> space >> nick)

lastSeen :: Parser CoucouCmd
lastSeen = do
    string "seen"
    some spaceChar
    n <- nick
    try (space >> eof >> return (CoucouCmdLastSeen n Nothing)) <|>
        do some spaceChar
           mbHl <- maybeHl
           end
           return $ CoucouCmdLastSeen n mbHl

getVersion :: Parser CoucouCmd
getVersion = do
    string "version"
    end
    return CoucouCmdVersion

tell :: Parser CoucouCmd
tell = do
    string "tell"
    some spaceChar
    n <- nick
    some spaceChar
    d <- tellDelay
    msg <- T.pack <$> some anyChar
    return $ CoucouCmdTell n msg d

tellDelay :: Parser (Maybe Timestamp)
tellDelay = try delay <|> return Nothing

delay :: Parser (Maybe Timestamp)
delay = do
    string "in"
    some spaceChar
    h <- fromMaybe 0 <$> delayCmd "hour"
    m <- fromMaybe 0 <$> delayCmd "minute"
    s <- fromMaybe 0 <$> delayCmd "second"
    return $ Just (max 0 (h * 3600 + m * 60 + s))
  where
    delayCmd :: String -> Parser (Maybe Integer)
    delayCmd sep = try (delayUnit sep) <|> return Nothing
    delayUnit :: String -> Parser (Maybe Integer)
    delayUnit sep = do
        mult <- option 1 (char '-' >> return (-1))
        d <- read <$> some digitChar
        some spaceChar
        string sep
        optional (char 's')
        some spaceChar
        return $ Just (mult * d)

see :: Parser CoucouCmd
see = do
    string "see"
    some spaceChar
    fact <- word >>= validFactoidName
    end
    return $ CoucouCmdFactoid fact SeeFactoids

search :: Parser CoucouCmd
search = do
    string "search"
    some spaceChar
    s <- T.pack <$> some anyChar
    end
    return $ CoucouCmdFactoid s SearchFactoids

end :: Parser ()
end = space <* eof
