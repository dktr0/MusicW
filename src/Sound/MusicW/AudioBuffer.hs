{-# LANGUAGE JavaScriptFFI #-}
module Sound.MusicW.AudioBuffer where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Control.Monad
import GHCJS.Prim (toJSArray, fromJSInt, getProp)

import Sound.MusicW.AudioContext
import Sound.MusicW.FloatArraySpec

newtype AudioBuffer = AudioBuffer JSVal
instance Show AudioBuffer where show _ = "an AudioBuffer"
instance PToJSVal AudioBuffer where pToJSVal (AudioBuffer val) = val
instance PFromJSVal AudioBuffer where pFromJSVal = AudioBuffer

newtype Float32Array = Float32Array JSVal
instance Show Float32Array where show _ = "a Float32Array"
instance PToJSVal Float32Array where pToJSVal (Float32Array val) = val
instance PFromJSVal Float32Array where pFromJSVal = Float32Array

instantiateArraySpec :: Either Float32Array FloatArraySpec -> IO Float32Array
instantiateArraySpec (Left f32Arr) = return f32Arr
instantiateArraySpec (Right spec) = do
  array <- js_createTypedArray $ arraySpecSize spec
  fillArray 0 spec array
  return array

fillArray :: Int -> FloatArraySpec -> Float32Array -> IO ()
fillArray _ EmptyArray _ = return ()
fillArray i (Const n x tl) arr = do
  js_typedArraySetConst i (i + n) x arr
  fillArray (i + n) tl arr
fillArray i (Segment xs tl) arr = do
  jsArray <- toJSArray $ fmap pToJSVal xs
  len <- fmap fromJSInt $ getProp jsArray "length"
  js_typedArraySet i jsArray arr
  fillArray (i + len) tl arr
fillArray i (Repeated rep xs tl) arr = do
  jsArray <- toJSArray $ fmap pToJSVal xs
  len <- fmap fromJSInt $ getProp jsArray "length"
  forM_ [0..rep-1] $ \it -> js_typedArraySet (i + (it * len)) jsArray arr
  fillArray (i + (rep * len)) tl arr


-- TODO: can all (or most?) of the "safe" declarations here be unsafe for efficiency?

foreign import javascript safe
  "if (Float32Array.prototype.fill !== void 0) { \
  \  $2.fill($1); \
  \} else { \
  \  for (var i = 0, len = a.length; i < len; i++) \
  \    $2[i] = $1; \
  \}"
  js_typedArrayFill :: Double -> Float32Array -> IO ()

foreign import javascript safe
  "if (Float32Array.prototype.fill !== void 0) { \
  \  $4.fill($3, $1, $2); \
  \} else { \
  \  for (var i = $1; i < Math.min($2, $4.length); i++) \
  \    $4[i] = $3; \
  \}"
  js_typedArraySetConst :: Int -> Int -> Double -> Float32Array -> IO ()
  -- fromIdx -> toIdx (exclusive) -> value -> array -> IO ()

foreign import javascript safe
  "$3.set($2, $1);"
  js_typedArraySet :: Int -> JSVal -> Float32Array -> IO ()
  -- startIdx -> fillFromJSArray -> array -> IO ()

foreign import javascript safe
  "$2[$1]"
  js_typedArrayGetAt :: Int -> Float32Array -> IO Double

foreign import javascript safe
  "$3[$1] = $2;"
  js_typedArraySetAt :: Int -> Double -> Float32Array -> IO ()

foreign import javascript safe
  "new Float32Array($1)"
  js_typedArrayFromArray :: JSVal -> IO Float32Array

foreign import javascript safe
  "new Float32Array($1)"
  js_createTypedArray :: Int -> IO Float32Array

foreign import javascript safe
  "$1.length"
  js_typedArrayLength :: Float32Array -> IO Int

foreign import javascript safe
  "$4.createBuffer($1, $2, $3)"
  js_createAudioBuffer :: Int -> Int -> Double -> AudioContext -> IO AudioBuffer
  -- numChannels -> length (in samples) -> sampleRate (frames/sec) -> ctx -> buffer

foreign import javascript safe
  "$1.getChannelData($2)"
  js_channelData :: AudioBuffer -> Int -> IO Float32Array
  -- buffer -> channel (0 indexed) -> data

foreign import javascript safe
  "$3.copyToChannel($1, $2);"
  js_copyToChannel :: Float32Array -> Int -> AudioBuffer -> IO ()

foreign import javascript safe
  "$1.length"
  js_bufferLength :: AudioBuffer -> IO Int

foreign import javascript safe
  "$1.duration"
  js_bufferDuration :: AudioBuffer -> IO AudioTime

foreign import javascript safe
  "$1.numberOfChannels"
  js_bufferNumChannels :: AudioBuffer -> IO Int

foreign import javascript safe
  "$1.inputBuffer"
  js_apeInputBuffer :: JSVal -> IO AudioBuffer

foreign import javascript safe
  "$1.outputBuffer"
  js_apeOutputBuffer :: JSVal -> IO AudioBuffer
