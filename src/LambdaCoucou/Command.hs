{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad (unless, mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
       (getFactoid, getFactoids, randomFactoid, searchFactoids, adjustCounterFactoid,
        setFactoid, resetFactoid, deleteFactoid, augmentFactoid)
import LambdaCoucou.Social
       (incCoucou, getCoucouCount, getLastSeen, coucouRank, registerTell,
        registerRemind)
import LambdaCoucou.Url (urlInfo)
import LambdaCoucou.Crypto (fetchCrypto)

handleCommand :: IRC.UnicodeEvent -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand ev cmd = unless (fromBlacklistedUser ev) $ handleCommand' ev cmd

handleCommand' :: IRC.UnicodeEvent -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand' _ T.CoucouCmdNop = return ()
handleCommand' ev cmd@(T.CoucouCmdCancer search mbHl) = do
    liftIO $ print cmd
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    let prefix = maybe "" (<> ": ") mbHl
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = prefix <> desc <> ": " <> url
            IRC.reply ev payload
handleCommand' ev cmd@(T.CoucouCmdFactoid name factoidType) = do
    liftIO $ print cmd
    case factoidType of
        T.GetFactoid mbHl -> prefixHlNick mbHl <$> getFactoid name >>= sendReply ev
        T.IncFactoid -> adjustCounterFactoid succ ev name
        T.DecFactoid -> adjustCounterFactoid pred ev name
        T.SetFactoid val -> setFactoid ev name val
        T.ResetFactoid val -> resetFactoid ev name val
        T.DeleteFactoid -> deleteFactoid ev name
        T.AugmentFactoid val -> augmentFactoid ev name val
        T.SeeFactoids -> getFactoids name >>= mapM_ (sendReply ev . Just)
        T.SearchFactoids mbSearch -> searchFactoids name mbSearch >>= mapM_ (sendReply ev . Just)
handleCommand' ev T.CoucouCmdRandomFactoid = randomFactoid >>= sendReply ev
handleCommand' ev T.CoucouCmdIncCoucou = incCoucou (IRC._source ev)
handleCommand' ev T.CoucouCmdCoucouRank = coucouRank (IRC._source ev) >>= IRC.reply ev
handleCommand' ev (T.CoucouCmdGetCoucou mbNick) =
    getCoucouCount (IRC._source ev) mbNick >>= sendReply ev
handleCommand' ev (T.CoucouCmdLastSeen nick mbHl) =
    prefixHlNick mbHl <$> getLastSeen nick >>= sendReply ev
handleCommand' ev T.CoucouCmdVersion = getBotVersion >>= IRC.reply ev
handleCommand' ev (T.CoucouCmdTell nick payload mbDelay) =
    registerTell ev nick payload mbDelay >>= sendReply ev
handleCommand' ev (T.CoucouCmdRemind nick payload mbDelay) =
    registerRemind ev nick payload mbDelay >>= sendReply ev
handleCommand' ev T.CoucouCmdUrl = urlInfo >>= IRC.reply ev
handleCommand' ev (T.CoucouCmdHelp mbCmd) = IRC.reply ev (helpCommand mbCmd)
handleCommand' ev (T.CoucouCmdCryptoRate sym hl) = fetchCrypto sym >>= sendReplyHl ev hl

prefixHlNick :: Maybe Text -> Maybe Text -> Maybe Text
prefixHlNick mbHl txt =
    let prefix = maybe "" (<> ": ") mbHl
    in (\t -> prefix <> t) <$> txt

fromBlacklistedUser :: IRC.UnicodeEvent -> Bool
fromBlacklistedUser ev =
    case IRC._source ev of
        IRC.Server _ -> False
        IRC.User nick -> nick `elem` blacklistedUsers
        IRC.Channel _ nick -> nick `elem` blacklistedUsers
  where
    blacklistedUsers = ["coucoubot"]

getBotVersion :: IRC.StatefulIRC T.BotState Text
getBotVersion = do
    state <- IRC.state
    let (sha, commitDate) = T._version state
    return $ pack $ "Commit " <> take 6 sha <> " (" <> commitDate <> ")"

sendReplyHl :: IRC.UnicodeEvent -> Maybe Text -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendReplyHl ev Nothing msg = sendReply ev msg
sendReplyHl ev (Just nick) msg = sendReply ev ((\m -> nick <> ": " <> m) <$> msg)

sendReply :: IRC.UnicodeEvent -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendReply _ Nothing = return ()
sendReply ev (Just msg) = IRC.reply ev msg

helpCommand :: Maybe T.CoucouHelpType -> Text
helpCommand Nothing = "List of commands: cancer, coucou, seen, tell, remind, version, factoid, random, url, crypto"
helpCommand (Just T.TypeCancer) =
    "cancer [search]: get a random cancer matching `search`. If no search is given, get any random cancer."
helpCommand (Just T.TypeCoucou) =
    "coucou [nick]: how many coucou for [nick]. If no nick is given, nick = sender of message."
helpCommand (Just T.TypeCoucouRank) = "List the best coucouteur"
helpCommand (Just T.TypeSeen) = "seen nick: when was the last time `nick` spoke ?"
helpCommand (Just T.TypeTell) =
    "tell nick [when] message: Next time `nick` speaks, tell her `message`. If a delay is given, wait at least this amount of time before delivering the message. Format of `when`: in [x hours] [y minutes] [z seconds]. Example: >tell Geekingfrog in 1 hour 2 minutes 10 seconds coucou"
helpCommand (Just T.TypeRemind) =
    "remind nick when message: Send `message` to `nick` in `when` time. Format of `when`: in [w weeks] [d days] [x hours] [y minutes] [z seconds]. Example: >remind me in 1 hour 2m 42 seconds coucou from the past."
helpCommand (Just T.TypeVersion) = "version: version of the bot."
helpCommand (Just T.TypeFactoid) =
    ">foo: get a random factoid named foo. >foo++ increment the foo counter by one. >foo := x reset factoid foo to value x. >foo += x adds x to the list of factoids for foo."
helpCommand (Just T.TypeRandom) = "get a random factoid"
helpCommand (Just (T.TypeUnknown term)) = "No command named " <> term <> "."
helpCommand (Just T.TypeUrl) = "Grab the title from the last url seen on the chan."
helpCommand (Just T.TypeCrypto) = "Exchange rate for Bitcoin (btc) and Ethereum (eth). Usage: crypto btc"
