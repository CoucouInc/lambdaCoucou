{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module LambdaCoucou.Crypto where

import           Control.Lens              ((^?))
import qualified Control.Monad.Except      as Ex
import           Control.Monad.IO.Class
import qualified Data.Aeson.Lens           as AL
import           Data.Scientific
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import           Network.HTTP.Req          ((/:))
import qualified Network.HTTP.Req          as Req
import qualified Network.IRC.Client        as IRC.C

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http         as LC.Http
import qualified LambdaCoucou.State        as LC.St

cryptoCommandHandler
  :: Either Text CryptoCoin
  -> Maybe Text
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

cryptoCommandHandler eiCoin mbTarget =
  case eiCoin of
    Left x -> pure $ Just $ showUnknownCoin x
    Right coin ->
      Just . LC.Hdl.addTarget mbTarget . either showCryptoError (showRate coin)
      <$> getRateInEuro coin

data CryptoError
  = CryptoNotFound CryptoCoin
  | HttpExc Req.HttpException
  deriving (Show)

showCryptoError :: CryptoError -> Text
showCryptoError = \case
  CryptoNotFound coin ->
    "Exchange rate not found for " <> symbol coin
  HttpExc exc ->
    LC.Http.showHttpException exc

newtype ReqMonad m a = ReqMonad { runReqMonad :: Ex.ExceptT CryptoError m a }
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError CryptoError, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError . HttpExc

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either CryptoError a)
runFetch = Ex.runExceptT . runReqMonad

data CryptoCoin
  = Ethereum
  | Bitcoin
  deriving (Show, Eq)

exchange :: CryptoCoin -> Text
exchange = \case
  Bitcoin -> "bitstamp"
  Ethereum -> "bitstamp"

symbol :: CryptoCoin -> Text
symbol = \case
  Bitcoin -> "btc"
  Ethereum -> "eth"


getRateInEuro
  :: (MonadIO m)
  => CryptoCoin
  -> m (Either CryptoError Scientific)

getRateInEuro coin = runFetch $ do
  let target = "eur"
  let pair = symbol coin <> target
  bsResp <- either Ex.throwError (pure . Req.responseBody) =<< runFetch
    (Req.req
      Req.GET
      (Req.https "api.cryptowat.ch" /: "markets" /: exchange coin /: pair /: "price")
      Req.NoReqBody
      Req.bsResponse
      mempty
    )

  case bsResp ^? AL.key "result" . AL.key "price" . AL._Number of
    Nothing -> Ex.throwError (CryptoNotFound coin)
    Just x  -> pure x

showRate :: CryptoCoin -> Scientific -> Text
showRate coin val
  = "1 "
  <> symbol coin
  <> " vaut "
  <> Tx.pack (show val)
  <> " euros grâce au pouvoir de la spéculation !"

showUnknownCoin :: Text -> Text
showUnknownCoin txt = "Dénomination inconnue: " <> txt <> ". Ici on ne deal qu'avec des monnaies respectueuses comme: btc and eth"
