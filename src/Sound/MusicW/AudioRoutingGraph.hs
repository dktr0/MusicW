module Sound.MusicW.AudioRoutingGraph where

import GHCJS.Types
import GHCJS.Prim(toJSString)
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback

newtype WebAudioContext = WebAudioContext JSVal
newtype AudioParam = AudioParam JSVal
newtype AudioBuffer = AudioBuffer JSVal
newtype Float32Array = Float32Array JSVal
newtype ArrayBuffer = ArrayBuffer JSVal

instance Show WebAudioContext where show _ = "WebAudioContext"
instance Show AudioParam where show _ = "AudioParam"
instance Show AudioBuffer where show _ = "AudioBuffer"
instance Show Float32Array where show _ = "Float32Array"
instance Show ArrayBuffer where show _ = "ArrayBuffer"

instance PToJSVal WebAudioContext where pToJSVal (WebAudioContext val) = val
instance PToJSVal AudioParam where pToJSVal (AudioParam val) = val
instance PToJSVal AudioBuffer where pToJSVal (AudioBuffer val) = val
instance PToJSVal Float32Array where pToJSVal (Float32Array val) = val
instance PToJSVal ArrayBuffer where pToJSVal (ArrayBuffer val) = val

instance PFromJSVal WebAudioContext where pFromJSVal = WebAudioContext
instance PFromJSVal AudioParam where pFromJSVal = AudioParam
instance PFromJSVal AudioBuffer where pFromJSVal = AudioBuffer
instance PFromJSVal Float32Array where pFromJSVal = Float32Array
instance PFromJSVal ArrayBuffer where pFromJSVal = ArrayBuffer

-- WebAudioContext functions

foreign import javascript safe
  "new (window.AudioContext || window.webkitAudioContext)()"
  js_newAudioContext :: IO WebAudioContext

foreign import javascript safe
  "window.___ac = $1;"
  js_setGlobalAudioContext :: WebAudioContext -> IO ()

foreign import javascript safe
  "$r = window.___ac;"
  js_globalAudioContext :: IO WebAudioContext

foreign import javascript safe
  "if (window.___ac == null) { \
  \    window.___ac = new (window.AudioContext || window.webkitAudioContext)();\
  \} $r = window.___ac;"
  js_setupGlobalAudioContext :: IO WebAudioContext

foreign import javascript safe
  "$1.currentTime"
  js_currentTime :: WebAudioContext -> IO Double
  -- time in seconds

foreign import javascript safe
  "$1.sampleRate"
  js_sampleRate :: WebAudioContext -> IO Double
  -- ctx -> sampleRate (frames/sec)


-- Node instantiation functions

foreign import javascript safe
  "$1.createOscillator()"
  js_createOscillator :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createBufferSource()"
  js_createBufferSource :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createBiquadFilter()"
  js_createBiquadFilter :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createConvolver()"
  js_createConvolver :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createDelay($2)"
  js_createDelay :: WebAudioContext -> Double -> IO JSVal

foreign import javascript safe
  "$1.createDynamicsCompressor()"
  js_createDynamicsCompressor :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createGain()"
  js_createGain :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createWaveShaper()"
  js_createWaveShaper :: WebAudioContext -> IO JSVal

foreign import javascript safe
  "$1.createScriptProcessor(void 0, $2, $3)"
  js_createScriptProcessor :: WebAudioContext -> Int -> Int -> IO JSVal
  -- ctx -> numInputChan -> numOutputChan -> IO node

foreign import javascript safe
  "$1.destination"
  js_destination :: WebAudioContext -> IO JSVal


-- Parameter functions

foreign import javascript safe
  "$1[$2] = $3;"
  js_setField :: JSVal -> JSVal -> JSVal -> IO ()

setJSField :: (PToJSVal o, PToJSVal v) => o -> String -> v -> IO ()
setJSField o f v = js_setField (pToJSVal o) (toJSString f) (pToJSVal v)

foreign import javascript safe
  "$1[$2]"
  js_audioParam :: JSVal -> JSVal -> AudioParam

foreign import javascript safe
  "$1.setValueAtTime($2, $3.currentTime);"
  js_setParamValue :: AudioParam -> Double -> WebAudioContext -> IO ()

foreign import javascript safe
  "$1.setValueAtTime($2, $3);"
  js_setParamValueAtTime :: AudioParam -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.linearRampToValueAtTime($2, $3);"
  js_linearRampToParamValueAtTime :: AudioParam -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.exponentialRampToValueAtTime($2, $3);"
  js_exponentialRampToParamValueAtTime :: AudioParam -> Double -> Double -> IO ()

foreign import javascript safe
  "$1.setValueCurveAtTime($2, $3, $4);"
  js_setParamValueCurveAtTime :: AudioParam -> Float32Array -> Double -> Double -> IO ()


-- Audio buffer/float32 array functions

foreign import javascript safe
  "$4.createBuffer($1, $2, $3)"
  js_createAudioBuffer :: Int -> Int -> Double -> WebAudioContext -> IO AudioBuffer
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
  js_bufferDuration :: AudioBuffer -> IO Double

foreign import javascript safe
  "$1.numberOfChannels"
  js_bufferNumChannels :: AudioBuffer -> IO Int

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


-- Node control functions

foreign import javascript safe
  "$1.connect($2);"
  js_connect :: JSVal -> JSVal -> IO ()

foreign import javascript safe
  "$1.disconnect($2);"
  js_disconnect :: JSVal -> JSVal -> IO ()

foreign import javascript safe
  "$1.disconnect();"
  js_disconnectAll :: JSVal -> IO ()

foreign import javascript safe
  "$1.start($2);"
  js_start :: JSVal -> Double -> IO ()

foreign import javascript safe
  "$1.stop($2);"
  js_stop :: JSVal -> Double -> IO ()


-- Event processing

foreign import javascript safe
  "$1.onended = $2;"
  js_onended :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "$1.onaudioprocess = $2;"
  js_onaudioprocess :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "$1.inputBuffer"
  js_apeInputBuffer :: JSVal -> IO AudioBuffer

foreign import javascript safe
  "$1.outputBuffer"
  js_apeOutputBuffer :: JSVal -> IO AudioBuffer


-- Utility

foreign import javascript safe
  "$1==null"
  js_isUndefined:: JSVal -> IO Bool
