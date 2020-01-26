{-# LANGUAGE EmptyCase #-}
module Control.Monad.Xhr.Base where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Xhr.Internal
import Control.Monad.Xhr.Types
import Data.Aeson (Value, FromJSON(..), eitherDecode)
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Bifunctor (first)
import Data.ByteString.Builder(toLazyByteString)
import Data.ByteString.Lazy as BSL
import Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)

class Monad m => MonadXhr m where
  callXhr :: XhrRequest -> m (Either XhrError XhrResponse)

newtype XhrT m a = XhrT
  { runXhrT :: ReaderT (XhrRequest -> m (Either XhrError XhrResponse)) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow)

xhrIO :: MonadIO m => XhrRequest -> m (Either XhrError XhrResponse)
xhrIO = flip runReaderT interpXhrImpl . runXhrT . callXhr

xhrValue :: (FromJSON a, MonadXhr m) => XhrRequest -> m (Either XhrError a)
xhrValue req = expect <$> callXhr req

xhrDecodeIO :: (FromJSON a, MonadIO m) => XhrRequest -> m (Either XhrError a)
xhrDecodeIO req = expect <$> xhrIO req

-- |@FromJSON@-style parser for XHR xrsponse
expectJSON :: (Value -> Parser a) -> Either XhrError XhrResponse -> Either XhrError a
expectJSON _ (Left err) = Left err
expectJSON checkJson (Right XhrResponse{..}) = do
  let
    decodeBody = \case
      XRSText txt         -> eitherDecode $ toLazyByteString (encodeUtf8Builder txt)
      XRSByteString bytes -> eitherDecode (BSL.fromStrict bytes)
  unless (xrsStatus >= 200 && xrsStatus < 300) $
    Left (BadStatus xrsStatus xrsStatusText)
  value <- first (BadResponse . T.pack) $ decodeBody xrsBody
  case parse checkJson value of
    Success a -> Right a
    Error err -> Left $ BadResponse (T.pack err)

-- |Check only status code
expectStatus :: (Int -> Bool) -> Either XhrError XhrResponse -> Either XhrError XhrResponse
expectStatus _ (Left err) = Left err
expectStatus checkStatus (Right xrsp@XhrResponse{..}) =
  if checkStatus xrsStatus then Right xrsp
  else Left $ BadStatus xrsStatus xrsStatusText

-- |Check status code is successfull i.e. inside 200..299 interval
expectStatus2xx :: Either XhrError XhrResponse -> Either XhrError XhrResponse
expectStatus2xx = expectStatus \s -> s >= 200 && s < 300

-- |Parse XHR xrsponse using 'FromJSON' instance of 'r'
expect :: forall r. FromJSON r => Either XhrError XhrResponse -> Either XhrError r
expect = expectJSON (parseJSON @r)

instance (Monad m) => MonadXhr (XhrT m) where
  callXhr req = XhrT $ ReaderT ($ req)

instance (MonadTrans XhrT) where
  lift = XhrT . lift

-- instance (MonadError e m) => MonadError e (XhrT m) where
--   throwError = XhrT . const . throwError
--   catchError ma ema = XhrT \a -> catchError (runXhrT ma a) (($ a) . runXhrT . ema)
