{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified LambdaCoucou.Url          as LC.Url

cancerCommandHandler
  :: CancerType
  -> Maybe Text
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

cancerCommandHandler cancer target = Ex.runExceptT (getCancer cancer) >>= \case
  Left err -> pure $ Just $ showCancerError err
  Right msg -> do
    LC.Url.updateLastUrl msg
    pure $ Just $ LC.Hdl.addTarget target msg

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
  :: (MonadIO m)
  => CancerType -> Ex.ExceptT CancerError m Text

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
  :: MonadIO m
  => Ex.ExceptT CancerError m (V.Vector (Text, Text))

fetchCancerList = do
  resp <- Ex.withExceptT HttpExc $ LC.Http.runReqMonad $ Req.req
    Req.GET
    (Req.https "raw.githubusercontent.com" /: "CoucouInc" /: "lalalaliste" /: "master" /: "cancer.txt")
    Req.NoReqBody
    Req.bsResponse
    mempty

  cancerLines <- case Tx.Enc.decodeUtf8' (Req.responseBody resp) of
    Left _err -> Ex.throwError DecodingError
    Right x   -> pure (Tx.lines x)
  pure $ V.fromList $ fmap parseLine cancerLines

parseLine :: Text -> (Text, Text)
parseLine l =
  let (k, v) = Tx.span (/= ':') l
  in (k, Tx.strip (Tx.drop 1 v))
