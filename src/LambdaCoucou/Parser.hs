{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Prelude hiding (fail)
import Control.Monad.Fail (fail)
import Data.Functor
import Data.Monoid ((<>))
import Data.Either
import Data.Foldable
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace, digitToInt)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import LambdaCoucou.Types (Timestamp)
import LambdaCoucou.Command (CoucouCmd(..), CmdFactoidType(..))
import LambdaCoucou.Crypto (CryptoCoin(..))
import LambdaCoucou.Help (CoucouHelpType(..))

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand = parse commandParser ""

-- if it doesn't start with the special character, ignore the command
commandParser :: Parser CoucouCmd
commandParser = try (prefix *> commandParser' <* space) <|> try incCoucou <|> pure CoucouCmdNop

prefix :: Parser Char
prefix = char 'λ' <|> char '&' <|> char 'Σ' -- Σ for sigma_g

commandParser' :: Parser CoucouCmd
commandParser' = choice
    [ try getVersion
    , try tell
    , try cryptoRate
    , try frenchDate
    , try remind
    , try getCoucou
    , try coucourank
    , try see
    , try search
    , try lastSeen
    , try help
    , try random
    , try cancer
    , try urlCommand
    , factoid
    ]

cancer :: Parser CoucouCmd
cancer = string "cancer"
    *> choice [eof $> CoucouCmdCancer Nothing Nothing, try justHl, normalCancer]
  where
    justHl = CoucouCmdCancer Nothing . Just <$> (spaceChar *> highlight)
    normalCancer = do
        spaces
        (s, hl) <- withHl' $ optional (T.unwords <$> sepEndBy1 word spaces)
        pure $ CoucouCmdCancer s hl

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
factoidBlackList = ["see", "search", "coucou", "seen", "cancer", "version", "tell", "remind", "random", "url"]

factoidName :: Maybe (Parser String) -> Parser Text
factoidName mbLimit = do
    let limit = fromMaybe (many spaceChar) mbLimit
    name <- T.pack <$> someTill (satisfy (not . isSpace)) (try $ lookAhead (some spaceChar <|> limit))
    if name `elem` factoidBlackList
    then fail $ T.unpack $ "Reserved keyword " <> name
    else return name

getFactoid :: Parser CoucouCmd
getFactoid = do
    (name, mbHl) <- withHl $ (word <* end)>>= validFactoidName
    pure $ CoucouCmdFactoid name (GetFactoid mbHl)


getCoucou :: Parser CoucouCmd
getCoucou = do
    string "coucou"
    target <-
        try (many spaceChar *> eof $> Nothing) -- (only space afterwards)
            <|> (some spaceChar >> (Just <$> nick)) -- (at least one space then a nick)
    return $ CoucouCmdGetCoucou target

coucourank :: Parser CoucouCmd
coucourank = string "coucourank" $> CoucouCmdCoucouRank

incCoucou :: Parser CoucouCmd
incCoucou = do
    space
    try (string "coucou" <* (eof <|> void spaceChar)) <|>
        manyTill anyChar (string " coucou" <* (eof <|> void (some spaceChar)))
    return CoucouCmdIncCoucou

nick :: Parser Text
nick = word

lastSeen :: Parser CoucouCmd
lastSeen = do
    string "seen"
    spaces
    (n, mbHl) <- withHl nick
    pure $ CoucouCmdLastSeen n mbHl

getVersion :: Parser CoucouCmd
getVersion = do
    string "version"
    end
    return CoucouCmdVersion

tell :: Parser CoucouCmd
tell = tellOrRemind "tell" CoucouCmdTell

remind :: Parser CoucouCmd
remind = tellOrRemind "remind" CoucouCmdRemind

delay :: Parser Timestamp
delay = do
    string "in"
    spaces
    ds <- sepEndBy1 time spaces
    pure $ sum ds

time :: Parser Integer
time = do
    d1 <- natural
    space
    mult <- choice
        [ try (week $> 3600 * 24 * 7)
        , try (day $> 3600 * 24)
        , try (hour $> 3600)
        , try (minute $> 60)
        , second $> 1
        ]
    pure $ d1 * mult

natural :: Parser Integer
natural = do
    ds <- some digitChar
    pure $ fromIntegral $ foldl' (\s d -> s * 10 + digitToInt d) 0 ds


week, day, hour, minute, second :: Parser ()
week = void $ choice [string "weeks", string "week", string "w"]
day = void $ choice [string "days", string "days", string "d"]
hour = void $ choice [string "hours", string "hour", string "h"]
minute = void $ choice [string "minutes", string "minute", string "m"]
second = void $ choice [string "seconds", string "second", string "s"]

tellOrRemind :: String -> (Text -> Text -> Maybe Timestamp -> CoucouCmd) -> Parser CoucouCmd
tellOrRemind str cmd = do
    string str
    some spaceChar
    n <- nick
    spaces
    d <- optional (try delay)
    msg <- T.pack <$> some anyChar
    pure $ cmd n msg d

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
    s <- word
    refined <- optional (try (some spaceChar >> word))
    end
    return $ CoucouCmdFactoid s (SearchFactoids refined)

help :: Parser CoucouCmd
help = do
    string "help"
    helpType <- helpTypeParser
    end
    return $ CoucouCmdHelp helpType
  where
    helpTypeParser = (try end >> return Nothing) <|> do
        some spaceChar
        term <- word
        end
        return $ Just $ case term of
            "cancer"  -> TypeCancer
            "coucou"  -> TypeCoucou
            "seen"    -> TypeSeen
            "tell"    -> TypeTell
            "remind"  -> TypeRemind
            "version" -> TypeVersion
            "random"  -> TypeRandom
            "url"     -> TypeUrl
            "crypto"  -> TypeCrypto
            "date"    -> TypeCalendar
            _         -> TypeUnknown term

random :: Parser CoucouCmd
random = string "random" >> return CoucouCmdRandomFactoid

end :: Parser ()
end = space <* eof

spaces :: Parser ()
spaces = skipSome spaceChar

word :: Parser Text
word = T.pack <$> some (satisfy (not . isSpace))

parseUrl :: Text -> Maybe Text
parseUrl raw = case parse url "" raw of
    Left _ -> Nothing
    Right mbUrl -> mbUrl

url :: Parser (Maybe Text)
url = (eof >> pure Nothing)
    <|> (many spaceChar >> (fmap Just (try url') <|> (word >> url)))

url' :: Parser Text
url' = do
    proto <- T.pack <$> (try (string "http://") <|> string "https://")
    rest <- word
    many anyChar
    return $ proto <> rest


urlCommand :: Parser CoucouCmd
urlCommand = string "url" *> end $> CoucouCmdUrl

cryptoRate :: Parser CoucouCmd
cryptoRate = do
    string "crypto"
    spaces
    (coin, hl) <- withHl' cryptoCoin
    pure $ CoucouCmdCryptoRate coin hl

cryptoCoin :: Parser CryptoCoin
cryptoCoin = choice
    [ (string "btc" <|> string "BTC") $> Bitcoin
    , (string "eth" <|> string "ETH") $> Ethereum
    ]

frenchDate :: Parser CoucouCmd
frenchDate = do
    (_, hl) <- withHl' (string "date" <* space)
    pure $ CoucouCmdCalendar hl

-- This parser isn't ideal because it loses all parse info when applying given parser p.
-- That's the only way I found to make sure the highlight info is correctly parsed
-- regardless of how greedy given parser p is.
withEndBy :: Parser b -> Parser a -> Parser (a, Maybe b)
withEndBy endP p = do
    (endResult, stuff) <- partitionEithers <$> many parseWithEnd <* eof
    let parsedEnd = case endResult of
            []    -> Nothing
            (x:_) -> Just x
    case parse p "inline" (T.pack stuff) of
        Left  err    -> fail $ show err
        Right result -> pure (result, parsedEnd)
  where
    parseWithEnd =
        try (Left <$> endP)
            <|> Right
            <$> (anyChar <* optional eof)

withHl, withHl' :: Parser a -> Parser (a, Maybe Text)
withHl = withEndBy highlight
withHl' = withEndBy (spaceChar *> highlight)

highlight :: Parser Text
highlight = char '>' *> space *> word <* space <* eof
