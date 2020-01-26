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
import GHCJS.Foreign.Callback as F
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal)
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

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "Control_Monad_Xhr_Internal.sendXhr($1, $2, $3);"
  send_xhr :: JSVal -> F.Callback (JSVal -> IO ()) -> F.Callback (JSVal -> IO ()) -> IO ()
#else
-- |Stub definitions for @intero-mode@
send_xhr :: JSVal -> F.Callback (JSVal -> IO ()) -> F.Callback (JSVal -> IO ()) -> IO ()
send_xhr = error "send_xhr: Unimplemented, only works in GHCJS"
#endif
