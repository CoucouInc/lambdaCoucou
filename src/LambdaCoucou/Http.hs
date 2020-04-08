module LambdaCoucou.Http where

import qualified Control.Monad.Except as Ex
import qualified Network.HTTP.Client as HTTP.C
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types.Status as HTTP.Status
import RIO
import qualified RIO.Text as T

showHttpException :: Req.HttpException -> Text
showHttpException = \case
  Req.JsonHttpException str -> T.pack str
  Req.VanillaHttpException exc -> case exc of
    HTTP.C.InvalidUrlException _url reason -> T.pack reason
    HTTP.C.HttpExceptionRequest _req content -> case content of
      HTTP.C.StatusCodeException rsp _body ->
        "Invalid status code ("
          <> T.pack (show $ HTTP.Status.statusCode $ HTTP.C.responseStatus rsp)
          <> ")"
      HTTP.C.TooManyRedirects _ -> "too many redirects"
      HTTP.C.ResponseTimeout -> "response timeout"
      HTTP.C.ConnectionTimeout -> "connection timeout"
      HTTP.C.ConnectionFailure _ -> "connection failure"
      other -> "weird HTTP error: " <> T.pack (show other)

-- TODO rework this module to simply throw exception
newtype ReqMonad m a = ReqMonad {runReqMonad :: Ex.ExceptT Req.HttpException m a}
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError Req.HttpException, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either Req.HttpException a)
runFetch = Ex.runExceptT . runReqMonad

runFetchEx :: (MonadIO m, MonadThrow m) => ReqMonad m a -> m a
runFetchEx action = Ex.runExceptT (runReqMonad action) >>= \case
  Left err -> throwIO err
  Right x -> pure x
