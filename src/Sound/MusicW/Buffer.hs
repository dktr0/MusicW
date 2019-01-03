module Sound.MusicW.Buffer (
  Buffer(..),
  BufferStatus(..),
  bufferStatus,
  createBuffer,
  startLoadingAndDecodingWithCallback,
  js_getAudioBuffer,
  js_createBufferFromURL,
  js_startLoadingAndDecodingMultiple,
  isBufferLoaded
) where

import Control.Monad.IO.Class
import Control.Monad (forM,liftM)
import Data.Map
import Data.JSString(JSString, unpack)

import GHCJS.DOM.File
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal.Internal
import GHCJS.Foreign.Callback

import Sound.MusicW.AudioContext
import Sound.MusicW.AudioBuffer

newtype Buffer = Buffer JSVal

instance Show Buffer where show _ = "a Buffer"

instance PToJSVal Buffer where pToJSVal (Buffer val) = val

instance ToJSVal Buffer where toJSVal (Buffer v) = return v
-- instance ToJSVal [Buffer] where toJSVal bufs= toJSValListOf

  --  toJSValListOf :: [a] -> IO JSVal

data BufferStatus
  = BufferUnderspecified
  | BufferLoading
  | BufferError String
  | BufferLoaded AudioBuffer

isBufferLoaded :: BufferStatus -> Bool
isBufferLoaded (BufferLoaded _) = True
isBufferLoaded _ = False

bufferStatus :: Buffer -> IO (Maybe BufferStatus)
bufferStatus buffer = do
  rawStatus <- js_getBufferStatus buffer
  case unpack rawStatus of
    "loading" -> return $ Just BufferLoading
    "decoding" -> return $ Just BufferLoading
    "error" -> do
      err <- js_getBufferError buffer
      return $ Just $ BufferError $ unpack err
    "decoded" -> do
      audioBuffer <- js_getAudioBuffer buffer
      return $ Just $ BufferLoaded audioBuffer
    otherwise -> return Nothing

createBuffer :: File -> IO Buffer
createBuffer file = do
  ctx <- getGlobalAudioContext
  js_createBuffer file ctx

startLoadingAndDecodingWithCallback :: Buffer -> (Buffer -> IO ()) -> IO ()
startLoadingAndDecodingWithCallback buffer evTrigger = do
  cb <- asyncCallback1 $ \buf -> evTrigger (Buffer buf)
  js_startLoadingAndDecoding buffer cb
  releaseCallback cb

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBuffer :: File -> AudioContext -> IO Buffer

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBufferFromURL :: JSString -> AudioContext -> IO Buffer

foreign import javascript safe
  "$1.startLoadingAndDecoding($2);"
  js_startLoadingAndDecoding :: Buffer -> Callback (JSVal -> IO ()) -> IO ()
  --                                                 ^ actually Buffer
foreign import javascript safe
  "startLoadingAndDecodingMultiple($1, $2)"
  js_startLoadingAndDecodingMultiple:: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "$1.status"
  js_getBufferStatus :: Buffer -> IO JSString

foreign import javascript safe
  "$1.error"
  js_getBufferError :: Buffer -> IO JSString

foreign import javascript safe
  "$1.buffer"
  js_getAudioBuffer :: Buffer -> IO AudioBuffer
