module Control.Monad.Xhr.Internal where

import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Xhr.Types
import Data.Bifunctor
import Data.String (fromString)
import Data.Text as T
import Data.Aeson
import Data.ByteString.Lazy as BSL
import Data.Text.Encoding as T
import Data.ByteString.Builder(toLazyByteString)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Simple as H
import qualified Network.HTTP.Types as H

-- | Run XHR effect using 'http-conduit'
interpXhrImpl :: MonadIO m => XhrRequest -> m (Either XhrError XhrResponse)
interpXhrImpl xrq =
  runExceptT
    $ bimapExceptT prepareError id
    $ syncIO
    $ fmap coerceResponse
    $ H.httpBS (coerceRequest xrq)
    where
      coerceRequest XhrRequest{..} =
        let baseReq = fromString (T.unpack xrqUrl) in
          baseReq
            { H.method         = fromString (show xrqMethod)
            , H.requestHeaders = prepareRequestHeaders xrqHeaders
            , H.requestBody    = H.RequestBodyBS $ case xrqBody of
                XRQText txt      -> T.encodeUtf8 txt
                XRQByteString bs -> bs
                XRQFile _        -> error "Sending XRQFile is not supported"
            }

      coerceResponse xresp = XhrResponse
        { xrsStatus     = H.statusCode (H.responseStatus xresp)
        , xrsStatusText = T.decodeUtf8 $ H.statusMessage (H.responseStatus xresp)
        , xrsBody       = XRSByteString (H.responseBody xresp)
        , xrsHeaders    = prepareResponseHeaders (H.responseHeaders xresp)
        , xrsUrl        = xrqUrl xrq }

      prepareError (SomeException ex) = NetworkError $ T.pack (show ex)
      prepareRequestHeaders = fmap $ bimap (fromString . T.unpack)  (fromString . T.unpack)
      prepareResponseHeaders = fmap $ bimap (T.pack . show)  (T.pack . show)

decodeBody :: FromJSON a => XhrResponseBody -> IO (Either String a)
decodeBody = \case
  XRSText txt         -> pure $ eitherDecode $ toLazyByteString (encodeUtf8Builder txt)
  XRSByteString bytes -> pure $ eitherDecode (BSL.fromStrict bytes)
