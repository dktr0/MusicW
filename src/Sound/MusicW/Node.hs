{-# LANGUAGE JavaScriptFFI #-}
module Sound.MusicW.Node where

import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Prim (toJSArray,toJSString)
import GHCJS.Foreign.Callback(Callback, asyncCallback1, releaseCallback)
import Control.Monad.IO.Class

import Sound.MusicW.AudioContext
import Sound.MusicW.AudioBuffer
import Sound.MusicW.FloatArraySpec

-- | A Node is an instantiated and wrapped node from the Web Audio API, or it is
-- a settable parameter of such a node. Nodes can potentially be connected and
-- disconnected to/from each other, started, stopped, and can have values/ramps/curves
-- scheduled on their parameters.

newtype Node = Node JSVal

instance PToJSVal Node where pToJSVal (Node j) = j

instance PFromJSVal Node where pFromJSVal = Node

instance Show Node where show _ = "a Node"

data ParamType
  = Frequency
  | Gain
  | Q
  | DelayTime
  | Threshold
  | Knee
  | CompressionRatio
  | Attack
  | Release
  -- obviously more are needed here...

instance Show ParamType where
  show Frequency = "frequency"
  show Gain = "gain"
  show Q = "Q"
  show DelayTime = "delayTime"
  show Threshold = "threshold"
  show Knee = "knee"
  show CompressionRatio = "ratio"
  show Attack = "attack"
  show Release = "release"

createConstantSource :: AudioIO m => Double -> m Node
createConstantSource v = do
  ctx <- audioContext
  node <- liftIO $ js_createConstantSource ctx
  liftIO $ js_setConstantOffset node v
  setNodeField node "isSource" True
  setNodeField node "isSink" False
  setNodeField node "startable" True

data OscillatorType
  = Sine
  | Square
  | Sawtooth
  | Triangle
  deriving (Show, Eq)

instance PToJSVal OscillatorType where
  pToJSVal Sine = pToJSVal "sine"
  pToJSVal Square = pToJSVal "square"
  pToJSVal Sawtooth = pToJSVal "sawtooth"
  pToJSVal Triangle = pToJSVal "triangle"

createOscillator :: AudioIO m => OscillatorType -> Double -> m Node
createOscillator t f = do
  ctx <- audioContext
  node <- liftIO $ js_createOscillator ctx
  setNodeField node "type" t
  setFrequency node f
  setNodeField node "isSource" True
  setNodeField node "isSink" False
  temp <- setNodeField node "startable" True
  return temp

data BufferParams = BufferParams Double Double Bool deriving (Show, Eq)

createAudioBufferSource :: AudioIO m => AudioBuffer -> BufferParams -> m Node
createAudioBufferSource buf params@(BufferParams loopstart loopend loop) = do
  ctx <- audioContext
  node <- liftIO $ js_createBufferSource ctx
  setNodeField node "buffer" buf
  setNodeField node "loopstart" loopstart
  setNodeField node "loopend" loopend
  setNodeField node "loop" loop
  setNodeField node "isSource" True
  setNodeField node "isSink" False
  setNodeField node "startable" True

data FilterSpec -- first argument always frequency, second argument when two is q or gain, when three: frequency q gain
  = LowPass Double Double
  | HighPass Double Double
  | BandPass Double Double
  | LowShelf Double Double
  | HighShelf Double Double
  | Peaking Double Double Double
  | Notch Double Double
  | AllPass Double Double
  deriving (Show)

-- temp note: filter types: LowPass, HighPass, BandPass,LowShelf,HighShelf,Peaking,Notch,AllPass
createBiquadFilter :: AudioIO m => FilterSpec -> m Node
createBiquadFilter spec = do
  ctx <- audioContext
  node <- liftIO $ js_createBiquadFilter ctx
  configureBiquadFilter spec node
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

configureBiquadFilter :: AudioIO m => FilterSpec -> Node -> m Node
configureBiquadFilter (LowPass f q) n = setNodeField n "type" "lowpass" >> setFrequency n f >> setQ n q
configureBiquadFilter (HighPass f q) n = setNodeField n "type" "highpass" >> setFrequency n f >> setQ n q
configureBiquadFilter (BandPass f q) n = setNodeField n "type" "bandpass" >> setFrequency n f >> setQ n q
configureBiquadFilter (LowShelf f g) n = setNodeField n "type" "lowshelf" >> setFrequency n f >> setGain n g
configureBiquadFilter (HighShelf f g) n = setNodeField n "type" "highshelf" >> setFrequency n f >> setGain n g
configureBiquadFilter (Peaking f q g) n = setNodeField n "type" "peaking" >> setFrequency n f >> setQ n q >> setGain n g
configureBiquadFilter (Notch f q) n = setNodeField n "type" "notch" >> setFrequency n f >> setQ n q
configureBiquadFilter (AllPass f q) n = setNodeField n "type" "allpass" >> setFrequency n f >> setQ n q

createConvolver :: AudioIO m => Either Float32Array FloatArraySpec -> Bool -> m Node
createConvolver bufferSpec normalize = do
  ctx <- audioContext
  node <- liftIO $ js_createConvolver ctx
  sr <- sampleRate
  bufferData <- liftIO $ instantiateArraySpec bufferSpec
  nSamples <- liftIO $ js_typedArrayLength bufferData
  buffer <- liftIO $ js_createAudioBuffer 1 nSamples sr ctx -- TODO (?) use a buffer spec instead of array spec (???)
  liftIO $ js_copyToChannel bufferData 1 buffer
  setNodeField node "buffer" buffer
  setNodeField node "normalize" normalize
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createDelay :: AudioIO m => AudioTime -> m Node
createDelay maxT = do
  ctx <- audioContext
  node <- liftIO $ js_createDelay ctx maxT
  setValue node DelayTime maxT
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createCompressor :: AudioIO m => Double -> Double -> Double -> AudioTime -> AudioTime -> m Node
createCompressor thr kne rat att rel = do
  ctx <- audioContext
  node <- liftIO $ js_createDynamicsCompressor ctx
  setValue node Threshold thr
  setValue node Knee kne
  setValue node CompressionRatio rat
  setValue node Attack att
  setValue node Release rel
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createGain :: AudioIO m => Double -> m Node
createGain g = do
  ctx <- audioContext
  node <- liftIO $ js_createGain ctx
  setValue node Gain g
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createChannelMerger :: AudioIO m => Int -> m Node
createChannelMerger nChnls = do
  ctx <- audioContext
  node <- liftIO $ js_createChannelMerger ctx nChnls
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createChannelSplitter :: AudioIO m => Int -> m Node
createChannelSplitter nChnls = do
  ctx <- audioContext
  node <- liftIO $ js_createChannelSplitter ctx nChnls
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

data OversampleAmount
  = NoOversampling
  | X2Oversampling
  | X4Oversampling
  deriving (Show)

instance PToJSVal OversampleAmount where
  pToJSVal NoOversampling = pToJSVal "none"
  pToJSVal X2Oversampling = pToJSVal "x2"
  pToJSVal X4Oversampling = pToJSVal "x4"

createWaveShaper :: AudioIO m => Either Float32Array FloatArraySpec -> OversampleAmount -> m Node
createWaveShaper curve oversample = do
  ctx <- audioContext
  node <- liftIO $ js_createWaveShaper ctx
  curveArray <- liftIO $ instantiateArraySpec curve
  setNodeField node "curve" curveArray
  setNodeField node "oversample" oversample
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" False

createScriptProcessor :: AudioIO m => Int -> Int -> (JSVal -> IO ()) -> m Node
createScriptProcessor inChnls outChnls cb = do
  ctx <- audioContext
  node <- liftIO $ js_createScriptProcessor ctx inChnls outChnls
  onaudioprocess <- liftIO $ asyncCallback1 cb
  liftIO $ js_onaudioprocess node onaudioprocess
  liftIO $ releaseCallback onaudioprocess
  setNodeField node "isSource" True
  setNodeField node "isSink" True
  setNodeField node "startable" True

createAnalyser :: AudioIO m => Int -> Double -> m Node
createAnalyser fftSize smoothingTimeConstant = do
  ctx <- audioContext
  node <- liftIO $ js_createAnalyser ctx
  setNodeField node "fftSize" fftSize
  setNodeField node "smoothingTimeConstant" smoothingTimeConstant
  setNodeField node "isSource" False -- technically, not true, but...
  setNodeField node "isSink" True
  setNodeField node "startable" False


-- | There is no function in the Web Audio API to "create" the context's
-- destination but we provide one anyway, as a convenient way to get a node
-- that, like all the other nodes, can be used in connections.


createDestination :: AudioIO m => m Node
createDestination = do
  node <- Node <$> destination
  setNodeField node "isSource" False
  setNodeField node "isSink" True
  setNodeField node "startable" False

createMicrophone :: AudioIO m => m Node
createMicrophone = do
  ctx <- audioContext
  node <- liftIO $ js_createMicrophone ctx
  setNodeField node "isSource" True
  setNodeField node "isSink" False
  setNodeField node "startable" False

createMediaStreamDestination :: AudioIO m => m Node
createMediaStreamDestination = do
  ctx <- audioContext
  node <- liftIO $ js_createMediaStreamDestination ctx
  setNodeField node "isSource" False
  setNodeField node "isSink" True
  setNodeField node "startable" False

getSharedMediaStreamDestination :: AudioIO m => m Node
getSharedMediaStreamDestination = do
  ctx <- audioContext
  node <- liftIO $ js_getContextSharedMediaStreamDestination ctx
  setNodeField node "isSource" False
  setNodeField node "isSink" True
  setNodeField node "startable" False

-- | Similarly, while the control parameters of nodes in the web audio API are distinct
-- we provide a function here to create a pseudo-node from a parameter of an
-- existing node. As with the destination (above), a node parameter, like a node,
-- can be something to which connections are made.

createParameter :: MonadIO m => Node -> ParamType -> m Node
createParameter node pType = do
  let p = Node $ js_audioParam node (pToJSVal $ show pType)
  setNodeField p "isSource" False
  setNodeField p "isSink" True
  setNodeField p "startable" False

connectNodes :: MonadIO m => Node -> Node -> m ()
connectNodes from to
  | not (js_isSource from) = error $ (show from) ++ " can't be connect source."
  | not (js_isSink to) = error $ (show to) ++ " can't be connect target."
  | otherwise   = liftIO $ js_connect from to

connectNodes' :: MonadIO m => Node -> Int -> Node -> Int -> m ()
connectNodes' fromNode fromChannel toNode toChannel
  | not (js_isSource fromNode) = error $ (show fromNode) ++ " can't be connect source."
  | not (js_isSink toNode) = error $ (show toNode) ++ " can't be connect target."
  | otherwise   = liftIO $ js_connect' fromNode toNode fromChannel toChannel

connectNodes'' :: MonadIO m => Node -> Int -> Node -> m ()
connectNodes'' fromNode fromChannel toNode
  | not (js_isSource fromNode) = error $ (show fromNode) ++ " can't be connect source."
  | not (js_isSink toNode) = error $ (show toNode) ++ " can't be connect target."
  | otherwise   = liftIO $ js_connect'' fromNode toNode fromChannel

disconnectNodes :: MonadIO m => Node -> Node -> m ()
disconnectNodes from to
  | not (js_isSource from) = error $ (show from) ++ " can't be disconnect source."
  | not (js_isSink to) == False = error $ (show to) ++ " can't be disconnect target."
  | otherwise   = liftIO $ js_disconnect from to

disconnectAll :: MonadIO m => Node -> m ()
disconnectAll x
  | not (js_isSource x) = return ()
  | otherwise  = liftIO $ js_disconnectAll x

startNode :: MonadIO m => AudioTime -> Node -> m ()
startNode t x
  | js_startable x = liftIO $ js_start x t
  | otherwise  = return ()

stopNode :: MonadIO m => AudioTime -> Node -> m ()
stopNode t x
  | js_startable x = liftIO $ js_stop x t
  | otherwise  = return ()

stopNodeNow :: MonadIO m => Node -> m ()
stopNodeNow x
  | js_startable x = liftIO $ js_stopNow x
  | otherwise = return ()

isSourceNode :: Node -> Bool
isSourceNode n = js_isSource n && (not $ js_isSink n)

isSinkNode :: Node -> Bool
isSinkNode n = js_isSink n && (not $ js_isSource n)


-- Definitions used to set/schedule the values of fields and parameters of nodes

setNodeField :: (MonadIO m, PToJSVal a) => Node -> String -> a -> m Node
setNodeField node field val = do
  liftIO $ js_setField (pToJSVal node) (pToJSVal field) (pToJSVal val)
  return node

setFrequency :: AudioIO m => Node -> Double -> m Node
setFrequency node = setValue node Frequency

setGain :: AudioIO m => Node -> Double -> m Node
setGain node = setValue node Gain

setQ :: AudioIO m => Node -> Double -> m Node
setQ node = setValue node Q

setValue :: AudioIO m => Node -> ParamType -> Double -> m Node
setValue node pType value = audioTime >>= setValueAtTime node pType value

setValueAtTime :: MonadIO m => Node -> ParamType -> Double -> AudioTime -> m Node
setValueAtTime node pType value time = do
  liftIO $ js_setValueAtTime node (pToJSVal $ show pType) value time
  return node

linearRampToValueAtTime :: MonadIO m => Node -> ParamType -> Double -> AudioTime -> m Node
linearRampToValueAtTime node pType value time = do
  liftIO $ js_linearRampToValueAtTime node (pToJSVal $ show pType) value time
  return node

exponentialRampToValueAtTime :: MonadIO m => Node -> ParamType -> Double -> AudioTime -> m Node
exponentialRampToValueAtTime node pType value time = do
  liftIO $ js_exponentialRampToValueAtTime node (pToJSVal $ show pType) value time
  return node

setValueCurveAtTime :: MonadIO m => Node -> ParamType -> [Double] -> AudioTime -> AudioTime -> m Node
setValueCurveAtTime node pType curve startTime duration = do
  curveArray <- liftIO $ toJSArray $ fmap pToJSVal curve
  typedCurveArray <- liftIO $ js_typedArrayFromArray curveArray
  liftIO $ js_setValueCurveAtTime node (pToJSVal $ show pType) typedCurveArray startTime duration
  return node

onended :: Node -> (JSVal -> IO ()) -> IO ()
onended n cb = do
  let n' = pToJSVal n
  onend <- asyncCallback1 $ \_ -> cb n'
  js_onended n onend
  releaseCallback onend


-- JS FFI functions (required, but not exported from this module):

foreign import javascript unsafe
  "$1.createConstantSource()"
  js_createConstantSource :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createOscillator()"
  js_createOscillator :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createBufferSource()"
  js_createBufferSource :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createBiquadFilter()"
  js_createBiquadFilter :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createConvolver()"
  js_createConvolver :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createDelay($2)"
  js_createDelay :: AudioContext -> AudioTime -> IO Node

foreign import javascript unsafe
  "$1.createDynamicsCompressor()"
  js_createDynamicsCompressor :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createGain()"
  js_createGain :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createChannelMerger($2)"
  js_createChannelMerger :: AudioContext -> Int -> IO Node

foreign import javascript unsafe
  "$1.createChannelSplitter($2)"
  js_createChannelSplitter :: AudioContext -> Int -> IO Node

foreign import javascript unsafe
  "$1.createWaveShaper()"
  js_createWaveShaper :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createScriptProcessor(void 0, $2, $3)"
  js_createScriptProcessor :: AudioContext -> Int -> Int -> IO Node

foreign import javascript safe
  "$r = $1.createGain();\
  \navigator.mediaDevices.getUserMedia({ audio: true, video: false}).then(function(stream) {\
  \  var x = new MediaStreamAudioSourceNode($1,{mediaStream: stream});\
  \  x.connect($r);\
  \});"
  js_createMicrophone :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createMediaStreamDestination()"
  js_createMediaStreamDestination :: AudioContext -> IO Node

foreign import javascript unsafe
  "if ($1.___stream == null) {\
  \  $1.___stream = $1.createMediaStreamDestination(); \
  \} $r = $1.___stream;"
  js_getContextSharedMediaStreamDestination :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.createAnalyser()"
  js_createAnalyser :: AudioContext -> IO Node

foreign import javascript unsafe
  "$1.connect($2);"
  js_connect :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.connect($2,$3,$4);"
  js_connect' :: Node -> Node -> Int -> Int -> IO ()

foreign import javascript unsafe
  "$1.connect($2,$3);"
  js_connect'' :: Node -> Node -> Int -> IO ()

foreign import javascript unsafe
  "$1.disconnect($2);"
  js_disconnect :: Node -> Node -> IO ()

foreign import javascript unsafe
  "$1.disconnect();"
  js_disconnectAll :: Node -> IO ()

foreign import javascript unsafe
  "$1.start($2);"
  js_start :: Node -> AudioTime -> IO ()

foreign import javascript unsafe
  "$1.stop($2);"
  js_stop :: Node -> AudioTime -> IO ()

foreign import javascript unsafe
  "$1.stop();"
  js_stopNow :: Node -> IO ()

foreign import javascript unsafe
  "$1.onaudioprocess = $2;"
  js_onaudioprocess :: Node -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1[$2].setValueAtTime($3, $4);"
  js_setValueAtTime :: Node -> JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe
  "$1[$2].linearRampToValueAtTime($3, $4);"
  js_linearRampToValueAtTime :: Node -> JSVal -> Double -> AudioTime -> IO ()

foreign import javascript unsafe
  "$1[$2].exponentialRampToValueAtTime($3, $4);"
  js_exponentialRampToValueAtTime :: Node -> JSVal -> Double -> AudioTime -> IO ()

foreign import javascript unsafe
  "$1[$2].setValueCurveAtTime($3, $4, $5);"
  js_setValueCurveAtTime :: Node -> JSVal -> Float32Array -> Double -> AudioTime -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3;"
  js_setField :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
  "$1.offset.value = $2"
  js_setConstantOffset :: Node -> Double -> IO ()

foreign import javascript unsafe
  "$1[$2]"
  js_audioParam :: Node -> JSVal -> JSVal

foreign import javascript unsafe
  "$1.onended = $2;"
  js_onended :: Node -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.isSource"
  js_isSource :: Node -> Bool

foreign import javascript unsafe
  "$1.isSink"
  js_isSink :: Node -> Bool

foreign import javascript unsafe
  "$1.startable"
  js_startable :: Node -> Bool

foreign import javascript unsafe
  "$1.numberOfInputs"
  numberOfInputs :: Node -> Int

foreign import javascript unsafe
  "$1.numberOfOutputs"
  numberOfOutputs :: Node -> Int
