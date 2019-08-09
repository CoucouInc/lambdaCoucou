{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module LambdaCoucou.Http where

import qualified Control.Monad.Except      as Ex
import           Control.Monad.IO.Class
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import qualified Network.HTTP.Client       as HTTP.C
import qualified Network.HTTP.Req          as Req
import qualified Network.HTTP.Types.Status as HTTP.Status

showHttpException :: Req.HttpException -> Text
showHttpException = \case
  Req.JsonHttpException str -> Tx.pack str
  Req.VanillaHttpException exc -> case exc of
    HTTP.C.InvalidUrlException _url reason -> Tx.pack reason
    HTTP.C.HttpExceptionRequest _req content -> case content of
      HTTP.C.StatusCodeException rsp _body
        -> "Invalid status code ("
        <> Tx.pack (show $ HTTP.Status.statusCode $ HTTP.C.responseStatus rsp)
        <> ")"
      HTTP.C.TooManyRedirects _ -> "too many redirects"
      HTTP.C.ResponseTimeout -> "response timeout"
      HTTP.C.ConnectionTimeout -> "connection timeout"
      HTTP.C.ConnectionFailure _ -> "connection failure"
      other -> "weird HTTP error: " <> Tx.pack (show other)


newtype ReqMonad m a = ReqMonad { runReqMonad :: Ex.ExceptT Req.HttpException m a }
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError Req.HttpException, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either Req.HttpException a)
runFetch = Ex.runExceptT . runReqMonad
