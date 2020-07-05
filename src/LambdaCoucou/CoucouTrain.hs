{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.CoucouTrain (coucouTrainHandler) where

import Data.Generics.Product.Fields (field)
import qualified GHC.Conc as TVar
import qualified LambdaCoucou.State as LC.St
import Lens.Micro.GHC ()
-- import the At instance for Map
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Lens as Lens
import qualified RIO.State as St
import qualified RIO.Text as T
import Say

coucouTrainHandler ::
  IRC.C.Source Text ->
  LC.St.ChannelName ->
  Text ->
  Text ->
  IRC.C.IRC LC.St.CoucouState ()
coucouTrainHandler source chan sourceNick msg = do
  instanceCfg <- asks (^. IRC.C.instanceConfig)
  ownNick <- (^. IRC.C.nick) <$> liftIO (TVar.readTVarIO instanceCfg)
  let stripped = T.strip msg
  let lowStripped = T.toLower stripped
  if stripped == "coucou " <> ownNick
    then do
      setCount 0
      IRC.C.replyTo source ("coucou " <> sourceNick)
    else do
      count <-
        fromMaybe 0
          <$> St.gets (^? field @"csChannels" . Lens.at chan . Lens._Just . field @"cstCoucouCount")

      say $ "Current count is: " <> tshow count
      if (any (`T.isPrefixOf` lowStripped) ["coucou", "cc"])
        then do
          if (count >= 3)
            then do
              IRC.C.replyTo source "coucou"
              setCount 0
            else setCount (count + 1)
        else setCount 0
  where
    setCount :: St.MonadState LC.St.CoucouState m => Int -> m ()
    setCount n =
      St.modify'
        ( \s ->
            s
              & field @"csChannels"
                . Lens.at chan
                . Lens._Just
                . field @"cstCoucouCount"
                .~ n
        )
