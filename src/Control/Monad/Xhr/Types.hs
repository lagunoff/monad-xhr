{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Xhr.Types where

import Data.ByteString
import Data.Default
import Data.Text
import GHC.Generics
import GHCJS.Marshal
import GHCJS.Marshal.Internal
import GHCJS.Types
import Unsafe.Coerce
import Control.Exception

#ifdef ghcjs_HOST_OS
import GHCJS.Buffer as Buffer
import GHCJS.Buffer.Types (SomeBuffer(..))
import GHC.Word
import Data.ByteString.Internal

instance PToJSVal ByteString where
  pToJSVal x = ((\(SomeBuffer x, offset, length) -> slice_array x (pToJSVal offset) (pToJSVal  (offset + length))) $ fromByteString x)
instance PFromJSVal ByteString where
  pFromJSVal = Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer . unsafeCoerce
#else
instance PToJSVal ByteString where pToJSVal = unsafeCoerce
instance PFromJSVal ByteString where pFromJSVal = unsafeCoerce
#endif

data Method = GET | POST | PUT | DELETE | PATCH
  deriving (Show, Eq, Generic, ToJSVal, FromJSVal)

type Url = Text

data XhrRequest = XhrRequest
  { xrqUrl             :: Url
  , xrqMethod          :: Method
  , xrqBody            :: XhrRequestBody
  , xrqHeaders         :: [(Text, Text)]
  , xrqWithCredentials :: Bool
  , xrqTimeout         :: Maybe Int
  , xrqResponseType    :: XhrResponseType
  } deriving (Generic, ToJSVal, FromJSVal)

instance Default XhrRequest where
  def = XhrRequest
    { xrqUrl             = "/"
    , xrqMethod          = GET
    , xrqHeaders         = []
    , xrqTimeout         = Nothing
    , xrqBody            = XRQText ""
    , xrqWithCredentials = False
    , xrqResponseType    = XRTText }

data XhrRequestBody
  = XRQText Text
  | XRQByteString ByteString
  | XRQFile JSVal
  deriving (Generic, ToJSVal, FromJSVal)

data XhrResponseType
  = XRTText
  | XRTArrayBuffer
  deriving (Generic, ToJSVal, FromJSVal)

data XhrResponse = XhrResponse
  { xrsUrl        :: !Text
  , xrsStatus     :: !Int
  , xrsStatusText :: !Text
  , xrsHeaders    :: ![(Text, Text)]
  , xrsBody       :: !XhrResponseBody
  } deriving (Generic, ToJSVal, FromJSVal)

data XhrResponseBody
  = XRSText !Text
  | XRSByteString !ByteString
  deriving (Generic, ToJSVal, FromJSVal)

data XhrError
  = BadUrl Text
  | BadPayload Text
  | NetworkError Text
  | Timeout
  | BadResponse Text
  | BadStatus Int Text
  deriving (Show, Eq, Generic, FromJSVal, ToJSVal, Exception)

instance ToJSVal ByteString where toJSVal = toJSVal_pure
instance FromJSVal ByteString where fromJSVal = fromJSVal_pure

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "(new Blob([$1.buf])).slice($2, $3)"
  slice_array :: JSVal -> JSVal -> JSVal -> JSVal
#else
slice_array :: JSVal -> JSVal -> JSVal -> JSVal
slice_array _ _ _ = undefined
#endif

