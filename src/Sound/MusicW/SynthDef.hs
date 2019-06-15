module Sound.MusicW.SynthDef where

import Control.Monad.State
import GHCJS.Types

import Sound.MusicW.AudioContext
import Sound.MusicW.FloatArraySpec
import Sound.MusicW.AudioBuffer
import Sound.MusicW.Node

data NodeRef
  = NodeRef Int
  | NodeInputRef Int Int -- for a numbered node, a specific numbered input of that node
  | NodeOutputRef Int Int -- for a numbered node, a specific numbered input of that node
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

instance Show (SynthSpec m) where
  show x = show (length $ nodeBuilders x) ++ " nodes, connections=" ++ show (connections x)
    ++ ", changes=" ++ show (changes x) ++ ", deletionTime=" ++ show (deletionTime x)

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

runSynthDef :: AudioIO m => SynthDef m a -> m (a, SynthSpec m)
runSynthDef x = runStateT x emptySynthSpec

addNodeBuilder :: Monad m => m Node -> SynthDef m NodeRef
addNodeBuilder x = do
  indexOfNewNode <- gets (length . nodeBuilders)
  modify $ \s -> s { nodeBuilders = nodeBuilders s ++ [x] }
  return $ NodeRef indexOfNewNode

connect :: Monad m => NodeRef -> NodeRef -> SynthDef m ()
connect from to = modify $ \s -> s { connections = connections s ++ [(from,to)] }

connect' :: Monad m => NodeRef -> Int -> NodeRef -> Int -> SynthDef m ()
connect' (NodeRef fromNode) fromIndex (NodeRef toNode) toIndex = connect (NodeOutputRef fromNode fromIndex) (NodeInputRef toNode toIndex)
connect' _ _ _ _ = error "unsupported connection type in connect'"

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

constantSource :: AudioIO m => Double -> SynthDef m NodeRef
constantSource x = addNodeBuilder $ createConstantSource x

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

channelMerger :: AudioIO m => [NodeRef] -> SynthDef m NodeRef
channelMerger xs = do
  y <- addNodeBuilder $ createChannelMerger (length xs)
  zipWithM_ (\x i -> connect' x 0 y i) xs [0..]
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

audioOut :: AudioIO m => NodeRef -> SynthDef m ()
audioOut input = connect input DestinationRef

audioIn :: AudioIO m => SynthDef m NodeRef
audioIn = addNodeBuilder $ createGain 1.0

resink :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
resink target input = connect input target >> return target

mix :: AudioIO m => [NodeRef] -> SynthDef m NodeRef
mix [] = constantSource 0 -- placeholder: we should have some kind of null node that translates into nothing for cases like this
mix (x:xs) = do
  y <- gain 1.0 x
  mapM (\n -> connect n y) xs
  return y

mixSynthDefs :: AudioIO m => [SynthDef m NodeRef] -> SynthDef m NodeRef
mixSynthDefs xs = sequence xs >>= mix

-- The final set of definitions here have to do with either connecting signals
-- to parameters of existing nodes, or setting/scheduling value changes/envelopes
-- on the parameters of existing nodes. The order of arguments is somewhat
-- inconsistent but is intended to facilitate common use cases where these
-- definitions are chained together.

param :: AudioIO m => ParamType -> NodeRef -> NodeRef -> SynthDef m ()
param pType (NodeRef i) input = connect input $ ParamRef i pType
param _ _ _ = error "connectParam used with not actual node"

setParam :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
setParam pType v t (NodeRef i) = addChange (SetValue (ParamRef i pType) v t) >> return (NodeRef i)
setParam _ _ _ _ = error "setParam used with not actual node"

linearRampOnParam :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
linearRampOnParam pType v t (NodeRef i) = addChange (LinearRampToValue (ParamRef i pType) v t) >> return (NodeRef i)
linearRampOnParam _ _ _ _ = error "linearRampOnParam used with not actual node"

exponentialRampOnParam :: AudioIO m => ParamType -> Double -> Double -> NodeRef -> SynthDef m NodeRef
exponentialRampOnParam pType v t (NodeRef i) = addChange (ExponentialRampToValue (ParamRef i pType) v t) >> return (NodeRef i)
exponentialRampOnParam _ _ _ _ = error "exponentialRampOnParam used with not actual node"

curveOnParam :: AudioIO m => ParamType -> [Double] -> Double -> Double -> NodeRef -> SynthDef m NodeRef
curveOnParam pType vs t dur (NodeRef i) = addChange (CurveToValue (ParamRef i pType) vs t dur) >> return (NodeRef i)
curveOnParam _ _ _ _ _ = error "curveOnParam used with not actual node"
