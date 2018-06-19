module Sound.MusicW.Node (
  WebAudioContext,
  Node(..),
  isSourceNode,
  isSinkNode,
  globalAudioContext,
  getCurrentTime,
  instantiateSourceNode,
  instantiateSourceSinkNode,
  instantiateSinkNode,
  audioParamNode,
  connect,
  disconnect,
  disconnectAll,
  start,
  stop,
  onended,
  setParamValueAtTime,
  linearRampToParamValueAtTime,
  exponentialRampToParamValueAtTime,
  setParamValueCurveAtTime
) where

import Sound.MusicW.AudioRoutingGraph hiding (Buffer)
import Sound.MusicW.Spec
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback(asyncCallback1, releaseCallback)
import GHCJS.Prim(JSVal, toJSArray, toJSString, fromJSInt, getProp)
import GHCJS.Types(nullRef)
import Control.Monad

data Node
  -- Source nodes
  = AudioBufferSourceNode { jsval :: JSVal, bufferParams :: BufferParams }  -- Also needs playbackParams b.c. some playback properties are only specified when you call 'start()' on that node
  | OscillatorNode { jsval :: JSVal }
  -- SourceSink nodes
  | BiquadFilterNode { jsval :: JSVal }
  | ConvolverNode { jsval :: JSVal }
  | DelayNode { jsval :: JSVal }
  | DynamicsCompressorNode { jsval :: JSVal }
  | GainNode { jsval :: JSVal }
  | WaveShaperNode { jsval :: JSVal }
  | ScriptProcessorNode { jsval :: JSVal }
  -- Sink nodes
  | DestinationNode { jsval :: JSVal }
  | AudioParamNode { jsval :: JSVal }

instance Show Node where
  show (AudioBufferSourceNode _ _) = "AudioBufferSourceNode"
  show (OscillatorNode _) = "OscillatorNode"
  show (BiquadFilterNode _) = "BiquadFilterNode"
  show (ConvolverNode _) = "ConvolverNode"
  show (DelayNode _) = "DelayNode"
  show (DynamicsCompressorNode _) = "DynamicsCompressorNode"
  show (GainNode _) = "GainNode"
  show (WaveShaperNode _) = "WaveShaperNode"
  show (ScriptProcessorNode _) = "ScriptProcessorNode"
  show (DestinationNode _) = "DestinationNode"
  show (AudioParamNode _) = "AudioParamNode"

isSourceNode :: Node -> Bool
isSourceNode (AudioBufferSourceNode _ _) = True
isSourceNode (OscillatorNode _) = True
isSourceNode _ = False

isSinkNode :: Node -> Bool
isSinkNode (DestinationNode _) = True
isSinkNode (AudioParamNode _) = True
isSinkNode _ = False

createAudioContext :: IO WebAudioContext
createAudioContext = js_newAudioContext

globalAudioContext :: IO WebAudioContext
globalAudioContext = js_setupGlobalAudioContext

getCurrentTime :: WebAudioContext -> IO Time
getCurrentTime ctx = js_currentTime ctx >>= return . Sec

setFrequencyHz :: JSVal -> Frequency -> WebAudioContext -> IO ()
setFrequencyHz node f = js_setParamValue (js_audioParam node $ toJSString "frequency") $ inHz f

setGainDb :: JSVal -> Gain -> WebAudioContext -> IO ()
setGainDb node g = js_setParamValue (js_audioParam node $ toJSString "gain") $ inDb g

setQ :: JSVal -> Double -> WebAudioContext -> IO ()
setQ node q = js_setParamValue (js_audioParam node $ toJSString "Q") q

instantiateSourceNode :: SourceNodeSpec -> WebAudioContext -> IO Node
instantiateSourceNode Silent ctx = do
  sampleRate <- js_sampleRate ctx
  buffer <- js_createAudioBuffer 1 (ceiling $ sampleRate * 10) sampleRate ctx
  channelData <- js_channelData buffer 0
  js_typedArrayFill 0.0 channelData
  src <- js_createBufferSource ctx
  setJSField src "buffer" buffer
  setJSField src "loop" True
  return $ AudioBufferSourceNode src (BufferParams 0 1 True)
instantiateSourceNode (Oscillator t f) ctx = do
  osc <- js_createOscillator ctx
  setJSField osc "type" t
  setFrequencyHz osc f ctx
  return $ OscillatorNode osc
instantiateSourceNode (AudioBufferSource buffer params@(BufferParams loopstart loopend loop)) ctx = do
  src <- js_createBufferSource ctx
  setJSField src "buffer" buffer
  setJSField src "loopstart" loopstart
  setJSField src "loopend" loopend
  setJSField src "loop" loop
  return $ AudioBufferSourceNode src params

instantiateSourceSinkNode :: SourceSinkNodeSpec -> WebAudioContext -> IO Node
instantiateSourceSinkNode (Filter spec) ctx = do
  biquadFilter <- js_createBiquadFilter ctx
  configureBiquadFilterNode spec biquadFilter ctx
  return $ BiquadFilterNode biquadFilter
instantiateSourceSinkNode (Convolver bufferSpec normalize) ctx = do
  convolver <- js_createConvolver ctx
  sampleRate <- js_sampleRate ctx
  bufferData <- instantiateArraySpec bufferSpec
  nSamples <- js_typedArrayLength bufferData
  buffer <- js_createAudioBuffer 1 nSamples sampleRate ctx -- TODO use a buffer spec instead of array spec
  js_copyToChannel bufferData 1 buffer
  js_setField convolver (toJSString "buffer") $ pToJSVal buffer
  js_setField convolver (toJSString "normalize") $ pToJSVal normalize
  return $ ConvolverNode convolver
instantiateSourceSinkNode (Delay t) ctx = do
  delay <- js_createDelay ctx $ inSec t -- createDelay needs a maxDelayTime
  js_setParamValue (js_audioParam delay (toJSString "delayTime")) (inSec t) ctx
  return $ DelayNode delay
instantiateSourceSinkNode (Compressor thr kne rat att rel) ctx = do
  comp <- js_createDynamicsCompressor ctx
  let setProp n v = js_setParamValue (js_audioParam comp (toJSString n)) v ctx
  setProp "threshold" $ inDb thr
  setProp "knee" $ inDb kne
  setProp "ratio" $ inDb rat
  setProp "attack" $ inSec att
  setProp "release" $ inSec rel
  return $ DynamicsCompressorNode comp
instantiateSourceSinkNode (Gain g) ctx = do
  gain <- js_createGain ctx
  js_setParamValue (js_audioParam gain (toJSString "gain")) (inAmp g) ctx
  return $ GainNode gain
instantiateSourceSinkNode (WaveShaper curve oversample) ctx = do
  shaper <- js_createWaveShaper ctx
  curveArray <- instantiateArraySpec curve
  js_setField shaper (toJSString "curve") $ pToJSVal curveArray
  js_setField shaper (toJSString "oversample") $ pToJSVal oversample
  return $ WaveShaperNode shaper
instantiateSourceSinkNode (DistortAt amp) ctx = do
  processor <- js_createScriptProcessor ctx 2 2 -- stereo in and out
  let clip = inAmp amp
  onaudioprocess <- asyncCallback1 $ \ape -> do
    input <- js_apeInputBuffer ape
    output <- js_apeOutputBuffer ape
    numSamples <- js_bufferLength input
    forM_ [0, 1] $ \chan -> do
      inData <- js_channelData input chan
      outData <- js_channelData output chan
      forM_ [0..numSamples] $ \sample -> do
        val <- js_typedArrayGetAt sample inData
        js_typedArraySetAt sample (max (-clip) $ min val clip) outData
  js_onaudioprocess processor onaudioprocess
  releaseCallback onaudioprocess
  return $ ScriptProcessorNode processor

configureBiquadFilterNode :: FilterSpec -> JSVal -> WebAudioContext -> IO ()
configureBiquadFilterNode (LowPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "lowpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (HighPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "highpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (BandPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "bandpass") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (LowShelf f g) node ctx =
  js_setField node (toJSString "type") (toJSString "lowshelf") >> setFrequencyHz node f ctx >> setGainDb node g ctx
configureBiquadFilterNode (HighShelf f g) node ctx =
  js_setField node (toJSString "type") (toJSString "highshelf") >> setFrequencyHz node f ctx >> setGainDb node g ctx
configureBiquadFilterNode (Peaking f q g) node ctx =
  js_setField node (toJSString "type") (toJSString "peaking") >> setFrequencyHz node f ctx >> setQ node q ctx >> setGainDb node g ctx
configureBiquadFilterNode (Notch f q) node ctx =
  js_setField node (toJSString "type") (toJSString "notch") >> setFrequencyHz node f ctx >> setQ node q ctx
configureBiquadFilterNode (AllPass f q) node ctx =
  js_setField node (toJSString "type") (toJSString "allpass") >> setFrequencyHz node f ctx >> setQ node q ctx

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

instantiateSinkNode :: SinkNodeSpec -> WebAudioContext -> IO Node
instantiateSinkNode Destination ctx = js_destination ctx >>= return . DestinationNode

audioParamNode :: Node -> String -> Node
audioParamNode node paramName =
  AudioParamNode $ pToJSVal $ js_audioParam (jsval node) (toJSString paramName)

connect :: Node -> Node -> IO ()
connect from to
  | isSinkNode from = error $ (show from) ++ " can't be connect source."
  | isSourceNode to = error $ (show to) ++ " can't be connect target."
  | otherwise   = js_connect (jsval from) (jsval to)

disconnect :: Node -> Node -> IO ()
disconnect from to
  | isSinkNode from = error $ (show from) ++ " can't be disconnect source."
  | isSourceNode to = error $ (show to) ++ " can't be disconnect target."
  | otherwise   = js_disconnect (jsval from) (jsval to)

disconnectAll :: Node -> IO ()
disconnectAll x
  | isSinkNode x = return ()
  | otherwise  = js_disconnectAll (jsval x)

start :: Time -> Node -> IO ()
start t x
  | isSourceNode x = js_start (jsval x) $ inSec t
  | otherwise  = return ()

stop :: Time -> Node -> IO ()
stop t x
  | isSourceNode x = js_stop (jsval x) $ inSec t
  | otherwise  = return ()

onended :: Node -> (Node -> IO ()) -> IO ()
onended n cb = do
  onend <- asyncCallback1 $ \_ -> cb n
  js_onended (jsval n) onend
  releaseCallback onend

setParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
setParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_setParamValueAtTime param value $ inSec time

linearRampToParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
linearRampToParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_linearRampToParamValueAtTime param value $ inSec time

exponentialRampToParamValueAtTime :: Node -> String -> Double -> Time -> IO ()
exponentialRampToParamValueAtTime node paramName value time = do
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_exponentialRampToParamValueAtTime param value $ inSec time

setParamValueCurveAtTime :: Node -> String -> [Double] -> Time -> Time -> IO ()
setParamValueCurveAtTime node paramName curve startTime duration = do
  curveArray <- toJSArray $ fmap pToJSVal curve
  typedCurveArray <- js_typedArrayFromArray curveArray
  let param = js_audioParam (jsval node) $ pToJSVal paramName
  js_setParamValueCurveAtTime param typedCurveArray (inSec startTime) (inSec duration)
