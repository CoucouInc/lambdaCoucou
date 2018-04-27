{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad (unless, mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
import LambdaCoucou.Social
       (incCoucou, getCoucouCount, getLastSeen, coucouRank, registerTell,
        registerRemind)
import LambdaCoucou.Url (urlInfo)
import LambdaCoucou.Crypto (fetchCrypto, CryptoCoin(..))
import LambdaCoucou.FrenchCalendar (getFrenchDate)
import LambdaCoucou.Help (helpCommand, CoucouHelpType)


-- TODO add a special command to report a parse error to a user.
-- This may be useful if the command is mispelled or something similar to give feedback
data CoucouCmd
    = CoucouCmdNop
    | CoucouCmdCancer !(Maybe Text)
                      !(Maybe Text) -- Maybe search substr, Maybe nick to hl
    | CoucouCmdFactoid !Text
                       CmdFactoidType
    | CoucouCmdRandomFactoid
    | CoucouCmdGetCoucou !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdIncCoucou
    | CoucouCmdCoucouRank
    | CoucouCmdLastSeen !Text
                        !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdVersion
    | CoucouCmdTell !Text !Text (Maybe T.Timestamp) -- nick payload maybe(delay)
    | CoucouCmdRemind !Text !Text (Maybe T.Timestamp) -- nick payload maybe(delay)
    | CoucouCmdHelp (Maybe CoucouHelpType)
    | CoucouCmdUrl
    | CoucouCmdCryptoRate CryptoCoin !(Maybe Text) -- symbol of the crypto to fetch, maybe nick to hl
    | CoucouCmdCalendar
    deriving (Eq)

data CmdFactoidType
    = GetFactoid !(Maybe Text) -- maybe user to hl
    | SetFactoid Text
    | ResetFactoid Text
    | AugmentFactoid Text
    | DeleteFactoid
    | IncFactoid
    | DecFactoid
    | SeeFactoids
    | SearchFactoids !(Maybe Text) -- maybe search term for values
    deriving (Eq, Show)


instance Show CoucouCmd where
    show CoucouCmdNop = "no op"
    -- show (CoucouCmdCoucou _) = "Counts of coucou"
    show (CoucouCmdCancer search mbHl) =
        case search of
            Nothing -> "A random cancer for nick " <> show mbHl <> "."
            Just s -> "The first cancer matching " <> unpack s <> " for " <> show mbHl <> "."
    show (CoucouCmdFactoid name factoidType) =
        let name' = unpack name
        in case factoidType of
               GetFactoid mbHl ->
                   "Get the factoid with name: " <> name' <> " and hl: " <> show mbHl <> "."
               SetFactoid val ->
                   "Set a new factoid with name: " <> name' <> " and val: " <> unpack val <> "."
               ResetFactoid val -> "Reset factoid: " <> name' <> " to val: " <> unpack val <> "."
               AugmentFactoid val ->
                   "Augment factoid: " <> name' <> " with val: " <> unpack val <> "."
               DeleteFactoid -> "Delete factoid: " <> name' <> "."
               IncFactoid -> "Increment counter: " <> name' <> "."
               DecFactoid -> "Decrement counter: " <> name' <> "."
               SeeFactoids -> "List factoids for name: " <> name' <> "."
               SearchFactoids mbSearch ->
                   "Search factoids with content: " <> name' <> " and refine search with " <> show mbSearch <>
                   "."
    show CoucouCmdRandomFactoid = "Get a random factoid"
    show (CoucouCmdGetCoucou (Just nick)) = "Show count of coucou for " <> unpack nick
    show (CoucouCmdGetCoucou Nothing) = "Show count of coucou for the sender of the message"
    show CoucouCmdIncCoucou = "Increment coucou count for sender of this message"
    show CoucouCmdCoucouRank = "Who has the most coucou"
    show (CoucouCmdLastSeen nick mbHl) =
        "How long since " <> unpack nick <> " has been seen ? (for user " <> show mbHl <> ")."
    show CoucouCmdVersion = "Send git info of the bot"
    show (CoucouCmdTell nick payload mbDelay) =
        "Tell " <> unpack nick <> ": " <> unpack payload <> " after " <> show mbDelay <> "s."
    show (CoucouCmdRemind nick payload mbDelay) =
        "Send " <> unpack nick <> ": " <> unpack payload <> " after " <> show mbDelay <> "s."
    show CoucouCmdUrl = "Grab infos from last url"
    show (CoucouCmdHelp Nothing) = "Help, list available commands."
    show (CoucouCmdHelp (Just t)) = "Help for the command " <> show t <> "."
    show (CoucouCmdCryptoRate coin _) = "Current rate for " <> show coin <> " in usd"
    show CoucouCmdCalendar = "Current date in the French Revolutionary calendar"



handleCommand :: IRC.UnicodeEvent -> CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand ev cmd = unless (fromBlacklistedUser ev) $ handleCommand' ev cmd

handleCommand' :: IRC.UnicodeEvent -> CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand' _ CoucouCmdNop = return ()
handleCommand' ev cmd@(CoucouCmdCancer search mbHl) = do
    liftIO $ print cmd
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    let prefix = maybe "" (<> ": ") mbHl
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = prefix <> desc <> ": " <> url
            IRC.reply ev payload
handleCommand' ev cmd@(CoucouCmdFactoid name factoidType) = do
    liftIO $ print cmd
    case factoidType of
        GetFactoid mbHl -> prefixHlNick mbHl <$> getFactoid name >>= sendReply ev
        IncFactoid -> adjustCounterFactoid succ ev name
        DecFactoid -> adjustCounterFactoid pred ev name
        SetFactoid val -> setFactoid ev name val
        ResetFactoid val -> resetFactoid ev name val
        DeleteFactoid -> deleteFactoid ev name
        AugmentFactoid val -> augmentFactoid ev name val
        SeeFactoids -> getFactoids name >>= mapM_ (sendReply ev . Just)
        SearchFactoids mbSearch -> searchFactoids name mbSearch >>= mapM_ (sendReply ev . Just)
handleCommand' ev CoucouCmdRandomFactoid = randomFactoid >>= sendReply ev
handleCommand' ev CoucouCmdIncCoucou = incCoucou (IRC._source ev)
handleCommand' ev CoucouCmdCoucouRank = coucouRank (IRC._source ev) >>= IRC.reply ev
handleCommand' ev (CoucouCmdGetCoucou mbNick) =
    getCoucouCount (IRC._source ev) mbNick >>= sendReply ev
handleCommand' ev (CoucouCmdLastSeen nick mbHl) =
    prefixHlNick mbHl <$> getLastSeen nick >>= sendReply ev
handleCommand' ev CoucouCmdVersion = getBotVersion >>= IRC.reply ev
handleCommand' ev (CoucouCmdTell nick payload mbDelay) =
    registerTell ev nick payload mbDelay >>= sendReply ev
handleCommand' ev (CoucouCmdRemind nick payload mbDelay) =
    registerRemind ev nick payload mbDelay >>= sendReply ev
handleCommand' ev CoucouCmdUrl = urlInfo >>= IRC.reply ev
handleCommand' ev (CoucouCmdHelp mbCmd) = IRC.reply ev (helpCommand mbCmd)
handleCommand' ev (CoucouCmdCryptoRate sym hl) = fetchCrypto sym >>= sendReplyHl ev hl
handleCommand' ev CoucouCmdCalendar = getFrenchDate >>= sendReply ev


prefixHlNick :: Maybe Text -> Maybe Text -> Maybe Text
prefixHlNick mbHl txt =
    let prefix = maybe "" (<> ": ") mbHl
    in (\t -> prefix <> t) <$> txt

fromBlacklistedUser :: IRC.UnicodeEvent -> Bool
fromBlacklistedUser ev = case IRC._source ev of
    IRC.Server _       -> False
    IRC.User   nick    -> nick `elem` blacklistedUsers
    IRC.Channel _ nick -> nick `elem` blacklistedUsers
    where blacklistedUsers = ["coucoubot"]

getBotVersion :: IRC.StatefulIRC T.BotState Text
getBotVersion = do
    state <- IRC.state
    let (sha, commitDate) = T._version state
    return $ pack $ "Commit " <> take 6 sha <> " (" <> commitDate <> ")"

sendReplyHl
    :: IRC.UnicodeEvent
    -> Maybe Text
    -> Maybe Text
    -> IRC.StatefulIRC T.BotState ()
sendReplyHl ev Nothing msg = sendReply ev msg
sendReplyHl ev (Just nick) msg =
    sendReply ev ((\m -> nick <> ": " <> m) <$> msg)

sendReply :: IRC.UnicodeEvent -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendReply _  Nothing    = return ()
sendReply ev (Just msg) = IRC.reply ev msg
