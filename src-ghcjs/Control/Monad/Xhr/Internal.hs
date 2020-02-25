{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE Unsafe, OverloadedStrings, JavaScriptFFI, CPP #-}
module Control.Monad.Xhr.Internal where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Xhr.Types
import Data.ByteString as BS
import GHC.Exts (Any)
import Data.Aeson as AE
import Data.Aeson.Types as AE
import GHCJS.Foreign.Callback as F
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal)
import Data.JSString as JSS
import Data.JSString.Text as JSS
import Language.Javascript.JSaddle
import Unsafe.Coerce

-- |Run XHR effect using @XmlHttpRequest@
interpXhrImpl :: MonadIO m => XhrRequest -> m (Either XhrError XhrResponse)
interpXhrImpl req = liftIO do
  mvar <- newEmptyMVar
  onSuccess <- F.asyncCallback1 \jsval -> do
    Just resp <- fromJSVal jsval
    putMVar mvar (Right resp)
  onFailure <- F.asyncCallback1 \jsval -> do
    Just err <- fromJSVal jsval
    putMVar mvar (Left err)
  jsRequest <- toJSVal req
  send_xhr jsRequest onSuccess onFailure
  readMVar mvar

decodeBody :: FromJSON a => XhrResponseBody -> JSM (Either String a)
decodeBody = \case
  XRSText txt     -> do
    jsvalTxt <- toJSVal txt
    jsvalJson <- json_parse jsvalTxt
    value <- fromJSValUnchecked @Value jsvalJson
    pure $ AE.parseEither parseJSON value
  XRSByteString _ -> error "decodeBody: not implemented for XRSByteString"

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "Control_Monad_Xhr_Internal.sendXhr($1, $2, $3);"
  send_xhr :: JSVal -> F.Callback (JSVal -> IO ()) -> F.Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "JSON.parse($1)"
  json_parse :: JSVal -> IO JSVal
#else
-- |Stub definitions for ghci
send_xhr :: JSVal -> F.Callback (JSVal -> IO ()) -> F.Callback (JSVal -> IO ()) -> IO ()
send_xhr = error "send_xhr: Unimplemented, only works in GHCJS"

json_parse :: JSVal -> JSM JSVal
json_parse = error "json_parse: Unimplemented, only works in GHCJS"
#endif
