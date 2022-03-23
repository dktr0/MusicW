module Sound.MusicW.SynthDef where

import Control.Monad.State
import GHCJS.Types

import Sound.MusicW.AudioContext
import Sound.MusicW.FloatArraySpec
import Sound.MusicW.AudioBuffer
import Sound.MusicW.Node

data NodeRef
  = NodeRef Int (Int,Int) -- tuplet is number of input and output channels for this node
  | NodeInputRef Int Int -- for a numbered node, a specific numbered input of that node
  | NodeOutputRef Int Int -- for a numbered node, a specific numbered output of that node
  | ExternalNodeRef Node (Int,Int) -- a node that prexists a synth instantiation, not disconnected etc when synth ends
  | ExternalNodeInputRef Node Int -- for an external node, a specific numbered input of that node
  | ExternalNodeOutputRef Node Int -- for an external node, a specific numbered output of that node
  | ParamRef Int ParamType
  | DestinationRef
  deriving (Show)

nodeRefInputCount :: NodeRef -> Int
nodeRefInputCount (NodeRef _ (i,_)) = i
nodeRefInputCount (NodeInputRef _ _) = 1
nodeRefInputCount (NodeOutputRef _ _) = 0
nodeRefInputCount (ExternalNodeRef _ (i,_)) = i
nodeRefInputCount (ExternalNodeInputRef _ _) = 1
nodeRefInputCount (ExternalNodeOutputRef _ _) = 0
nodeRefInputCount (ParamRef _ _) = 1
nodeRefInputCount DestinationRef = 2 -- hard-coding of stereo output here might be problematic or irrelevant...

nodeRefOutputCount :: NodeRef -> Int
nodeRefOutputCount (NodeRef _ (_,o)) = o
nodeRefOutputCount (NodeInputRef _ _) = 0
nodeRefOutputCount (NodeOutputRef _ _) = 1
nodeRefOutputCount (ExternalNodeRef _ (_,o)) = o
nodeRefOutputCount (ExternalNodeInputRef _ _) = 0
nodeRefOutputCount (ExternalNodeOutputRef _ _) = 1
nodeRefOutputCount (ParamRef _ _) = 0
nodeRefOutputCount DestinationRef = 0

data Change
  = SetValue { paramRef :: NodeRef , value :: Double, endTime :: AudioTime }
  | LinearRampToValue { paramRef :: NodeRef , value :: Double, endTime :: AudioTime }
  | ExponentialRampToValue { paramRef :: NodeRef, value :: Double, endTime :: AudioTime }
  | CurveToValue { paramRef :: NodeRef, values :: [Double], startTime :: AudioTime, duration :: AudioTime }
  deriving (Show)

data SynthSpec m = SynthSpec {
  nodeBuilders :: [m Node],
  connections :: [(NodeRef,NodeRef)],
  changes :: [Change],
  deletionTime :: Maybe AudioTime
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

addNodeBuilder :: Monad m => (Int,Int) -> m Node -> SynthDef m NodeRef
addNodeBuilder (iCount,oCount) x = do
  indexOfNewNode <- gets (length . nodeBuilders)
  modify $ \s -> s { nodeBuilders = nodeBuilders s ++ [x] }
  return $ NodeRef indexOfNewNode (iCount,oCount)

connect :: Monad m => NodeRef -> NodeRef -> SynthDef m ()
connect from to = modify $ \s -> s { connections = connections s ++ [(from,to)] }

-- given two NodeRefs that are either NodeRef or ExternalNodeRef, connect specific inputs and outputs of them to eachother
connect' :: Monad m => NodeRef -> Int -> NodeRef -> Int -> SynthDef m ()
connect' (NodeRef from _) fromIndex (NodeRef to _) toIndex = connect (NodeOutputRef from fromIndex) (NodeInputRef to toIndex)
connect' (NodeRef from _) fromIndex (ExternalNodeRef to _) toIndex = connect (NodeOutputRef from fromIndex) (ExternalNodeInputRef to toIndex)
connect' (ExternalNodeRef from _) fromIndex (NodeRef to _) toIndex = connect (ExternalNodeOutputRef from fromIndex) (NodeInputRef to toIndex)
connect' (ExternalNodeRef from _) fromIndex (ExternalNodeRef to _) toIndex = connect (ExternalNodeOutputRef from fromIndex) (ExternalNodeInputRef to toIndex)
connect' _ _ _ _ = error "unsupported connection type in connect'"

addChange :: Monad m => Change -> SynthDef m ()
addChange x = modify $ \s -> s { changes = changes s ++ [x] }

setDeletionTime :: Monad m => Maybe AudioTime -> SynthDef m ()
setDeletionTime t = modify $ \s -> s { deletionTime = combineDeletionTimes (deletionTime s) t }

combineDeletionTimes :: Maybe AudioTime -> Maybe AudioTime -> Maybe AudioTime
combineDeletionTimes Nothing Nothing = Nothing
combineDeletionTimes (Just t) Nothing = Just t
combineDeletionTimes Nothing (Just t) = Just t
combineDeletionTimes (Just t1) (Just t2) = Just (max t1 t2)


-- definitions for nodes that are only sources (no NodeRefs as arguments)

constantSource :: AudioIO m => Double -> SynthDef m NodeRef
constantSource x = addNodeBuilder (0,1) $ createConstantSource x

oscillator :: AudioIO m => OscillatorType -> Double -> SynthDef m NodeRef
oscillator t f = addNodeBuilder (0,1) $ createOscillator t f

audioBufferSource :: AudioIO m => AudioBuffer -> BufferParams -> SynthDef m NodeRef
audioBufferSource buf ps = addNodeBuilder (0,1) $ createAudioBufferSource buf ps -- hard-coding of 1 output channel could become problematic

-- for the following definitions, relating to nodes that are sinks, the penultimate
-- type for each is a NodeRef, so that Synth computations can be chained together
-- (x >>= y >>= z) in such a way that the node of x becomes an input to y, whose node
-- becomes an input to z.

biquadFilter :: AudioIO m => FilterSpec -> NodeRef -> SynthDef m NodeRef
biquadFilter x input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createBiquadFilter x
  connect input y
  return y

gain :: AudioIO m => Double -> NodeRef -> SynthDef m NodeRef
gain x input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createGain x
  connect input y
  return y

channelMerger :: AudioIO m => [NodeRef] -> SynthDef m NodeRef
channelMerger xs = do
  y <- addNodeBuilder (length xs,length xs) $ createChannelMerger (length xs)
  zipWithM_ (\x i -> connect' x 0 y i) xs [0..]
  return y

channelSplitter :: AudioIO m => NodeRef -> SynthDef m [NodeRef]
channelSplitter input | nodeRefOutputCount input < 1 = error "MusicW: channelSplitter called on NodeRef not representing a node output"
                      | otherwise = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (1,nchnls) $ createChannelSplitter nchnls
  let (NodeRef n _) = y
  connect input y
  return $ fmap (NodeOutputRef n) [0 .. (nchnls-1)]

convolver :: AudioIO m => Either Float32Array FloatArraySpec -> Bool -> NodeRef -> SynthDef m NodeRef
convolver spec normalize input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createConvolver spec normalize
  connect input y
  return y

delay :: AudioIO m => AudioTime -> NodeRef -> SynthDef m NodeRef
delay maxT input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createDelay maxT
  connect input y
  return y

compressor :: AudioIO m => Double -> Double -> Double -> AudioTime -> AudioTime -> NodeRef -> SynthDef m NodeRef
compressor thr kne rat att rel input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createCompressor thr kne rat att rel
  connect input y
  return y

waveShaper :: AudioIO m => Either Float32Array FloatArraySpec -> OversampleAmount -> NodeRef -> SynthDef m NodeRef
waveShaper curve oversample input = do
  let nchnls = nodeRefOutputCount input
  y <- addNodeBuilder (nchnls,nchnls) $ createWaveShaper curve oversample
  connect input y
  return y

scriptProcessor :: AudioIO m => Int -> Int -> (JSVal -> IO ()) -> NodeRef -> SynthDef m NodeRef
scriptProcessor inChnls outChnls cb input = do
  y <- addNodeBuilder (inChnls,outChnls) $ createScriptProcessor inChnls outChnls cb
  connect input y
  return y

analyser :: AudioIO m => Int -> Double -> NodeRef -> SynthDef m NodeRef
analyser fftSize smoothingTimeConstant input = do
  y <- addNodeBuilder (1,0) $ createAnalyser fftSize smoothingTimeConstant
  connect input y
  return y

audioOut :: AudioIO m => NodeRef -> SynthDef m ()
audioOut input = connect input DestinationRef

audioIn :: AudioIO m => SynthDef m NodeRef
audioIn = addNodeBuilder (0,1) $ createGain 1.0 -- TODO: hard-coded single input channel will be problematic later

microphone :: AudioIO m => SynthDef m NodeRef
microphone = addNodeBuilder (0,1) createMicrophone

externalNode :: AudioIO m => Node -> SynthDef m NodeRef
externalNode x = do
  let iChnls = numberOfInputs x
  let oChnls = numberOfOutputs x
  return $ ExternalNodeRef x (iChnls,oChnls)

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
param pType (NodeRef i (_,_)) input = connect input $ ParamRef i pType
param _ _ _ = error "connectParam used with not actual node"

setParam :: AudioIO m => ParamType -> Double -> AudioTime -> NodeRef -> SynthDef m NodeRef
setParam pType v t (NodeRef i (iChnls,oChnls)) = addChange (SetValue (ParamRef i pType) v t) >> return (NodeRef i (iChnls,oChnls))
setParam _ _ _ _ = error "setParam used with not actual node"

linearRampOnParam :: AudioIO m => ParamType -> Double -> AudioTime -> NodeRef -> SynthDef m NodeRef
linearRampOnParam pType v t (NodeRef i (iChnls,oChnls)) = addChange (LinearRampToValue (ParamRef i pType) v t) >> return (NodeRef i (iChnls,oChnls))
linearRampOnParam _ _ _ _ = error "linearRampOnParam used with not actual node"

exponentialRampOnParam :: AudioIO m => ParamType -> Double -> AudioTime -> NodeRef -> SynthDef m NodeRef
exponentialRampOnParam pType v t (NodeRef i (iChnls,oChnls)) = addChange (ExponentialRampToValue (ParamRef i pType) v t) >> return (NodeRef i (iChnls,oChnls))
exponentialRampOnParam _ _ _ _ = error "exponentialRampOnParam used with not actual node"

curveOnParam :: AudioIO m => ParamType -> [Double] -> AudioTime -> AudioTime -> NodeRef -> SynthDef m NodeRef
curveOnParam pType vs t dur (NodeRef i (iChnls,oChnls)) = addChange (CurveToValue (ParamRef i pType) vs t dur) >> return (NodeRef i (iChnls,oChnls))
curveOnParam _ _ _ _ _ = error "curveOnParam used with not actual node"
