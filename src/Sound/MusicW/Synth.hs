module Sound.MusicW.Synth (
  Synth(..),
  playSynth,
  playSynthNow,
  nodeRefToNode,
  synthDefToSynth,
  synthDefToSynth_,
  startSynth,
  startSynthNow,
  stopSynth,
  stopSynthNow,
  disconnectSynth,
  restartSynth
  ) where

import Data.Foldable (find)
import Control.Monad (void)
import Control.Monad.IO.Class

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef

data Synth m = Synth {
  cachedDestination :: Node,
  spec :: SynthSpec m,
  nodes :: [Node]
  --audioBuffers :: Map Int AudioBuffer, -- TODO restarting a AudioBufferSourceNode needs to replace the buffers
  } deriving (Show)

playSynth :: AudioIO m => Node -> AudioTime -> SynthDef m a -> m (a, Synth m)
playSynth dest t x = do
  (a,spec) <- runSynthDef x
  s <- synthSpecToSynth dest spec
  startSynth t s
  return (a,s)

playSynthNow :: AudioIO m => Node -> SynthDef m a -> m (a, Synth m)
playSynthNow dest x = do
  (a,spec) <- runSynthDef x
  s <- synthSpecToSynth dest spec
  startSynthNow s
  return (a,s)

nodeRefToNode :: AudioIO m => NodeRef -> Synth m -> m Node
nodeRefToNode (NodeRef i (_,_)) s = return $ (nodes s)!!i
nodeRefToNode (ParamRef i pType) s = createParameter ((nodes s)!!i) pType
nodeRefToNode DestinationRef s = return $ cachedDestination s

synthDefToSynth :: AudioIO m => Node -> SynthDef m a -> m (Synth m)
synthDefToSynth dest x = execSynthDef x >>= synthSpecToSynth dest

synthDefToSynth_ :: AudioIO m => SynthDef m a -> m (Synth m)
synthDefToSynth_ x = createDestination >>= (flip synthDefToSynth) x

synthSpecToSynth :: AudioIO m => Node -> SynthSpec m -> m (Synth m)
synthSpecToSynth dest x = do
  ns <- sequence $ nodeBuilders x
  mapM_ (uncurry (makeConnections dest ns)) $ connections x
  disconnectOnStop ns
  return $ Synth { cachedDestination = dest, spec = x, nodes = ns }


makeConnections :: AudioIO m => Node -> [Node] -> NodeRef -> NodeRef -> m ()

-- connections to (deferred) destination
makeConnections dest ns (NodeRef from _) DestinationRef = connectNodes (ns!!from) dest
makeConnections dest ns (NodeOutputRef from fromIndex) DestinationRef = connectNodes'' (ns!!from) fromIndex dest
makeConnections dest ns (ExternalNodeRef from _) DestinationRef = connectNodes from dest
makeConnections dest ns (ExternalNodeOutputRef from fromIndex) DestinationRef = connectNodes'' from fromIndex dest

-- connections to NodeRef
makeConnections _ ns (NodeRef from _) (NodeRef to _) = connectNodes (ns!!from) (ns!!to)
makeConnections _ ns (NodeOutputRef from fromIndex) (NodeRef to _) = connectNodes'' (ns!!from) fromIndex (ns!!to)
makeConnections _ ns (ExternalNodeRef from _) (NodeRef to _) = connectNodes from (ns!!to)
makeConnections _ ns (ExternalNodeOutputRef from fromIndex) (NodeRef to _) = connectNodes'' from fromIndex (ns!!to)

-- connections to ExternalNodeRef
makeConnections _ ns (NodeRef from _) (ExternalNodeRef to _) = connectNodes (ns!!from) to
makeConnections _ ns (NodeOutputRef from fromIndex) (ExternalNodeRef to _) = connectNodes'' (ns!!from) fromIndex to
makeConnections _ ns (ExternalNodeRef from _) (ExternalNodeRef to _) = connectNodes from to
makeConnections _ ns (ExternalNodeOutputRef from fromIndex) (ExternalNodeRef to _) = connectNodes'' from fromIndex to

-- connections to ParamRef
makeConnections _ ns (NodeRef from _) (ParamRef to pType) = createParameter (ns!!to) pType >>= connectNodes (ns!!from)
makeConnections _ ns (NodeOutputRef fromNode fromChannel) (ParamRef to pType) = createParameter (ns!!to) pType >>= connectNodes'' (ns!!fromNode) fromChannel
makeConnections _ ns (ExternalNodeRef from _) (ParamRef to pType) = createParameter (ns!!to) pType >>= connectNodes from
makeConnections _ ns (ExternalNodeOutputRef from fromIndex) (ParamRef to pType) = createParameter (ns!!to) pType >>= connectNodes'' from fromIndex

-- connections to NodeInputRef (note that connections from NodeRef/ExternalNodeRef not supported)
makeConnections _ ns (NodeOutputRef from fromIndex) (NodeInputRef to toIndex) = connectNodes' (ns!!from) fromIndex (ns!!to) toIndex
makeConnections _ ns (ExternalNodeOutputRef from fromIndex) (NodeInputRef to toIndex) = connectNodes' from fromIndex (ns!!to) toIndex

-- connections to ExternalNodeInputRef (note that connections from NodeRef/ExternalNodeRef not supported)
makeConnections _ ns (NodeOutputRef from fromIndex) (ExternalNodeInputRef to toIndex) = connectNodes' (ns!!from) fromIndex to toIndex
makeConnections _ ns (ExternalNodeOutputRef from fromIndex) (ExternalNodeInputRef to toIndex) = connectNodes' from fromIndex to toIndex


-- *** Note: there is probably a bug connected to the definition of disconnectOnStop below:
-- it attaches an onended callback to a single source node from a list of nodes, which doesn't
-- properly cover the case where there are multiple source nodes that end at different times.

disconnectOnStop :: (Foldable t, AudioIO m) => t Node -> m ()
disconnectOnStop ns = maybe (return ()) f $ find isSourceNode ns
  where f x = liftIO $ onended x $ \_ -> mapM_ disconnectAll ns

startSynth :: AudioIO m => AudioTime -> Synth m -> m ()
startSynth t0 s = do
  mapM_ (startNode t0) $ nodes s
  maybe (return ()) (\t -> stopSynth (t0+t) s) $ deletionTime (spec s)
  mapM_ (scheduleChange (nodes s) t0) $ changes (spec s)

scheduleChange :: AudioIO m => [Node] -> AudioTime -> Change -> m ()
scheduleChange ns t0 (SetValue (ParamRef i pType) v t) = void $ setValueAtTime (ns!!i) pType v (t0+t)
scheduleChange ns t0 (LinearRampToValue (ParamRef i pType) v t) = void $ linearRampToValueAtTime (ns!!i) pType v (t0+t)
scheduleChange ns t0 (ExponentialRampToValue (ParamRef i pType) v t) = void $ exponentialRampToValueAtTime (ns!!i) pType v (t0+t)
scheduleChange ns t0 (CurveToValue (ParamRef i pType) curve t dur) = void $ setValueCurveAtTime (ns!!i) pType curve (t0+t) dur
scheduleChange _ _ _ = error "scheduleChange targeted non-ParamRef node"

startSynthNow :: AudioIO m => Synth m -> m ()
startSynthNow s = do
  t <- audioTime
  startSynth (t + 0.050) s

stopSynth :: MonadIO m => AudioTime -> Synth m -> m ()
stopSynth t s = mapM_ (stopNode t) $ nodes s

stopSynthNow :: MonadIO m => Synth m -> IO ()
stopSynthNow s = mapM_ stopNodeNow $ nodes s

disconnectSynth :: MonadIO m => Synth m -> IO ()
disconnectSynth s = mapM_ disconnectAll $ nodes s

restartSynth :: AudioIO m => AudioTime -> Synth m -> m ()
restartSynth t s = do
  stopSynth t s
  s' <- synthSpecToSynth (cachedDestination s) (spec s)
  startSynth t s'
