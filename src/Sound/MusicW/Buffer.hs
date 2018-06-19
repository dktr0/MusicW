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

import Sound.MusicW.AudioRoutingGraph

newtype Buffer = Buffer JSVal

instance Show Buffer where show _ = "Buffer"

instance PToJSVal Buffer where pToJSVal (Buffer val) = val

instance ToJSVal Buffer where toJSVal (Buffer v) = return v
-- instance ToJSVal [Buffer] where toJSVal bufs= toJSValListOf

  --  toJSValListOf :: [a] -> IO JSVal

data BufferStatus
  = BufferUnderspecified
  | BufferLoading
  | BufferError String
  | BufferLoaded AudioBuffer

isBufferLoaded::BufferStatus -> Bool
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





-- mapToBuffer :: MonadWidget t m => Event t File -> m (Event t Buffer, Event t BufferStatus)
-- mapToBuffer fileEv = do
--   -- bufferEv :: Event t Buffer - a buffer ready to start loading it's file
--   bufferEv <- performEvent $ ffor fileEv $ \file -> liftIO $ do
--     ctx <- js_setupGlobalAudioContext
--     js_createBuffer file ctx
--
--   -- stateChangeEv :: Event t Buffer - tiggered on a status change
--   stateChangeEv <- performEventAsync $ ffor bufferEv $ \buffer evTrigger -> liftIO $ do
--     cb <- asyncCallback1 $ \buf -> evTrigger (Buffer buf)
--     js_startLoadingAndDecoding buffer cb
--     releaseCallback cb

createBuffer :: File -> IO Buffer
createBuffer file = do
  ctx <- js_setupGlobalAudioContext
  js_createBuffer file ctx

startLoadingAndDecodingWithCallback :: Buffer -> (Buffer -> IO ()) -> IO ()
startLoadingAndDecodingWithCallback buffer evTrigger = do
  cb <- asyncCallback1 $ \buf -> evTrigger (Buffer buf)
  js_startLoadingAndDecoding buffer cb
  releaseCallback cb

-- -- | buffer creates a smart buffer for asynchronous loading of the most recent `Just` file fired
-- -- from the `Event t (Maybe File)`. Until the first occurance of the event, the buffer is `Nothing`.
-- -- The returned buffer status monitors the current state of the buffer.
-- buffer :: MonadWidget t m => Event t (Maybe File) -> m (Dynamic t (Maybe Buffer), Dynamic t BufferStatus)
-- buffer maybeFileEv = do
--   (bufferEv, statusEv) <- mapToBuffer (fmapMaybe id maybeFileEv)
--   dynBuffer <- holdDyn Nothing $ fmap Just bufferEv
--   dynStatus <- holdDyn BufferUnderspecified statusEv
--   return (dynBuffer, dynStatus)

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBuffer :: File -> WebAudioContext -> IO Buffer

foreign import javascript safe
  "new Buffer($1, $2)"
  js_createBufferFromURL :: JSString -> WebAudioContext -> IO Buffer


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
