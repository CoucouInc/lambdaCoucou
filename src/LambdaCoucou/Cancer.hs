{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Wwarn #-}

module LambdaCoucou.Cancer where

import qualified Control.Monad.Except      as Ex
import           Control.Monad.IO.Class
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import qualified Data.Text.Encoding        as Tx.Enc
import           Data.Vector               ((!))
import qualified Data.Vector               as V
import           Network.HTTP.Req          ((/:))
import qualified Network.HTTP.Req          as Req
import qualified Network.IRC.Client        as IRC.C
import qualified System.Random             as Rng

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http         as LC.Http
import qualified LambdaCoucou.State        as LC.St

cancerCommandHandler
  :: CancerType
  -> Maybe Text
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

cancerCommandHandler cancer target
  = Just . LC.Hdl.addTarget target . either showCancerError id
  <$> runFetch (getCancer cancer)

newtype ReqMonad m a = ReqMonad { runReqMonad :: Ex.ExceptT CancerError m a }
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError CancerError, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError . HttpExc

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either CancerError a)
runFetch = Ex.runExceptT . runReqMonad

data CancerError
  = HttpExc Req.HttpException
  | CancerNotFound Text
  | ParseError
  | DecodingError
  deriving (Show)

showCancerError :: CancerError -> Text
showCancerError = \case
  HttpExc exc ->
    LC.Http.showHttpException exc
  CancerNotFound txt ->
    "No cancer found for: " <> txt -- TODO levenstein distance for some suggestions?
  ParseError ->
    "Could not parse the returned list"
  DecodingError ->
    "Returned list is not in valid utf8"

data CancerType
  = RandomCancer
  | SpecificCancer Text
  deriving (Show, Eq)

getCancer
  :: ( MonadIO m
     , Ex.MonadError CancerError m
     , Req.MonadHttp m
     )
  => CancerType
  -> m Text

getCancer cancer = do
  list <- fetchCancerList
  case cancer of
    RandomCancer -> do
      idx <- liftIO $ Rng.randomRIO (0, V.length list - 1)
      let (k, v) = list ! idx
      pure $ k <> ": " <> v
    SpecificCancer c -> do
      let lowC = Tx.toLower c
      case V.find (Tx.isInfixOf lowC . Tx.toLower . fst) list of
        Nothing -> Ex.throwError (CancerNotFound c)
        Just (k, v)  -> pure $ k <> ": " <> v


fetchCancerList
  :: ( MonadIO m
     , Ex.MonadError CancerError m
     , Req.MonadHttp m
     )
  => m (V.Vector (Text, Text))

fetchCancerList = do
  resp <- Req.req
    Req.GET
    (Req.https "raw.githubusercontent.com" /: "CoucouInc" /: "lalalaliste" /: "master" /: "cancer.txt")
    Req.NoReqBody
    Req.bsResponse
    mempty
  lines <- case Tx.Enc.decodeUtf8' (Req.responseBody resp) of
    Left _err -> Ex.throwError DecodingError
    Right x   -> pure (Tx.lines x)
  pure $ V.fromList $ fmap parseLine lines

parseLine :: Text -> (Text, Text)
parseLine l =
  let (k, v) = Tx.span (/= ':') l
  in (k, Tx.strip (Tx.drop 1 v))
