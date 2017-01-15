{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Data.Monoid ((<>))
import Control.Monad (unless, mapM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids
       (getFactoid, getFactoids, searchFactoids, adjustCounterFactoid,
        setFactoid, resetFactoid, deleteFactoid, augmentFactoid)
import LambdaCoucou.Social
       (incCoucou, getCoucouCount, getLastSeen, registerTell,
        registerRemind)

handleCommand :: IRC.UnicodeEvent -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand _ T.CoucouCmdNop = return ()
handleCommand ev cmd@(T.CoucouCmdCancer search mbHl) = do
    liftIO $ print cmd
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    let prefix = fromMaybe "" ((<> ": ") <$> mbHl)
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = prefix <> desc <> ": " <> url
            IRC.reply ev payload
handleCommand ev cmd@(T.CoucouCmdFactoid name factoidType) = do
    liftIO $ print cmd
    unless (fromBlacklistedUser ev) $
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
handleCommand ev T.CoucouCmdIncCoucou = incCoucou (IRC._source ev)
handleCommand ev (T.CoucouCmdGetCoucou mbNick) =
    getCoucouCount (IRC._source ev) mbNick >>= sendReply ev
handleCommand ev (T.CoucouCmdLastSeen nick mbHl) =
    prefixHlNick mbHl <$> getLastSeen nick >>= sendReply ev
handleCommand ev T.CoucouCmdVersion = getBotVersion >>= IRC.reply ev
handleCommand ev (T.CoucouCmdTell nick payload mbDelay) =
    registerTell ev nick payload mbDelay >>= sendReply ev
handleCommand ev (T.CoucouCmdRemind nick payload mbDelay) =
    registerRemind ev nick payload mbDelay >>= sendReply ev

prefixHlNick :: Maybe Text -> Maybe Text -> Maybe Text
prefixHlNick mbHl txt =
    let prefix = fromMaybe "" ((<> ": ") <$> mbHl)
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

sendReply :: IRC.UnicodeEvent -> Maybe Text -> IRC.StatefulIRC T.BotState ()
sendReply _ Nothing = return ()
sendReply ev (Just msg) = IRC.reply ev msg
