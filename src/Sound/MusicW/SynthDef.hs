module Sound.MusicW.SynthDef where

import Control.Monad.State
import GHCJS.Types

import Sound.MusicW.AudioContext
import Sound.MusicW.FloatArraySpec
import Sound.MusicW.AudioBuffer
import Sound.MusicW.Node

data ParamType
  = Freq
  | Gain
  -- obviously more are needed here...

instance Show ParamType where
  show Freq = "freq"

data NodeRef
  = NodeRef Int
  | ParamRef Int ParamType
  | DestinationRef
  deriving (Show)

data Change
  = SetValue { paramRef :: NodeRef , value :: Double, endTime :: Double }
  | LinearRampToValue { paramRef :: NodeRef , value :: Double, endTime :: Double }
  | ExponentialRampToValue { paramRef :: NodeRef, value :: Double, endTime :: Double }
  | CurveToValue { paramRef :: NodeRef, values :: [Double], startTime :: Double, duration :: Double }
  deriving (Show)

data SynthSpec m = SynthSpec {
  nodeBuilders :: [m Node],
  connections :: [(NodeRef,NodeRef)],
  changes :: [Change],
  deletionTime :: Maybe Double
  }

emptySynthSpec :: SynthSpec m
emptySynthSpec = SynthSpec {
  nodeBuilders = [],
  connections = [],
  changes = [],
  deletionTime = Nothing
  }

type SynthDef m a = StateT (SynthSpec m) m a

execSynthDef :: AudioIO m => SynthDef m a -> m (SynthSpec m)
execSynthDef x = execStateT x emptySynthSpec

addNodeBuilder :: Monad m => m Node -> SynthDef m NodeRef
addNodeBuilder x = do
  indexOfNewNode <- gets (length . nodeBuilders)
  modify $ \s -> s { nodeBuilders = nodeBuilders s ++ [x] }
  return $ NodeRef indexOfNewNode

connect :: Monad m => NodeRef -> NodeRef -> SynthDef m ()
connect from to = modify $ \s -> s { connections = connections s ++ [(from,to)] }

addChange :: Monad m => Change -> SynthDef m ()
addChange x = modify $ \s -> s { changes = changes s ++ [x] }

setDeletionTime :: Monad m => Maybe Double -> SynthDef m ()
setDeletionTime t = modify $ \s -> s { deletionTime = combineDeletionTimes (deletionTime s) t }

combineDeletionTimes :: Maybe Double -> Maybe Double -> Maybe Double
combineDeletionTimes Nothing Nothing = Nothing
combineDeletionTimes (Just t) Nothing = Just t
combineDeletionTimes Nothing (Just t) = Just t
combineDeletionTimes (Just t1) (Just t2) = Just (max t1 t2)


-- definitions for nodes that are only sources (no NodeRefs as arguments)

constant :: AudioIO m => Double -> SynthDef m NodeRef
constant x = addNodeBuilder $ createConstantSource x

oscillator :: AudioIO m => OscillatorType -> Double -> SynthDef m NodeRef
oscillator t f = addNodeBuilder $ createOscillator t f

audioBufferSource :: AudioIO m => AudioBuffer -> BufferParams -> SynthDef m NodeRef
audioBufferSource buf ps = addNodeBuilder $ createAudioBufferSource buf ps

-- for the following definitions, relating to nodes that are sinks, the penultimate
-- type for each is a NodeRef, so that Synth computations can be chained together
-- (x >>= y >>= z) in such a way that the node of x becomes an input to y, whose node
-- becomes an input to z.

biquadFilter :: AudioIO m => FilterSpec -> NodeRef -> SynthDef m NodeRef
biquadFilter x input = do
  y <- addNodeBuilder $ createBiquadFilter x
  connect input y
  return y

gain :: AudioIO m => Double -> NodeRef -> SynthDef m NodeRef
gain x input = do
  y <- addNodeBuilder $ createGain x
  connect input y
  return y

convolver :: AudioIO m => Either Float32Array FloatArraySpec -> Bool -> NodeRef -> SynthDef m NodeRef
convolver spec normalize input = do
  y <- addNodeBuilder $ createConvolver spec normalize
  connect input y
  return y

delay :: AudioIO m => Double -> NodeRef -> SynthDef m NodeRef
delay maxT input = do
  y <- addNodeBuilder $ createDelay maxT
  connect input y
  return y

compressor :: AudioIO m => Double -> Double -> Double -> Double -> Double -> NodeRef -> SynthDef m NodeRef
compressor thr kne rat att rel input = do
  y <- addNodeBuilder $ createCompressor thr kne rat att rel
  connect input y
  return y

waveShaper :: AudioIO m => Either Float32Array FloatArraySpec -> OversampleAmount -> NodeRef -> SynthDef m NodeRef
waveShaper curve oversample input = do
  y <- addNodeBuilder $ createWaveShaper curve oversample
  connect input y
  return y

scriptProcessor :: AudioIO m => Int -> Int -> (JSVal -> IO ()) -> NodeRef -> SynthDef m NodeRef
scriptProcessor inChnls outChnls cb input = do
  y <- addNodeBuilder $ createScriptProcessor inChnls outChnls cb
  connect input y
  return y

out :: AudioIO m => NodeRef -> SynthDef m ()
out input = connect input DestinationRef

-- The final set of definitions here have to do with either connecting signals
-- to parameters of existing nodes, or setting/scheduling value changes/envelopes
-- on the parameters of existing nodes. The order of arguments is somewhat
-- inconsistent but is intended to facilitate common use cases where these
-- definitions are chained together.

connectToParam :: AudioIO m => ParamType -> NodeRef -> NodeRef -> SynthDef m ()
connectToParam pType (NodeRef i) input = connect input $ ParamRef i pType
connectToParam _ _ _ = error "connectToParam used with not actual node"

setParamValue :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
setParamValue pType v t (NodeRef i) = addChange (SetValue (ParamRef i pType) v t) >> return (NodeRef i)
setParamValue _ _ _ _ = error "setParamValue used with not actual node"

linearRampToParamValue :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
linearRampToParamValue pType v t (NodeRef i) = addChange (LinearRampToValue (ParamRef i pType) v t) >> return (NodeRef i)
linearRampToParamValue _ _ _ _ = error "linearRampToParamValue used with not actual node"

exponentialRampToParamValue :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
exponentialRampToParamValue pType v t (NodeRef i) = addChange (ExponentialRampToValue (ParamRef i pType) v t) >> return (NodeRef i)
exponentialRampToParamValue _ _ _ _ = error "exponentialRampToParamValue used with not actual node"

curveToParamValue :: AudioIO m => ParamType -> [Double] -> Double -> Double -> NodeRef -> SynthDef m NodeRef
curveToParamValue pType vs t dur (NodeRef i) = addChange (CurveToValue (ParamRef i pType) vs t dur) >> return (NodeRef i)
curveToParamValue _ _ _ _ _ = error "curveToParamValue used with not actual node"
