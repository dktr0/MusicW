module Sound.MusicW.Synth (
  Synth(..),
  playSynth_,
  playSynth,
  startSynth,
  startSynthNow,
  stopSynth,
  stopSynthNow,
  restartSynth
  ) where

import Data.Foldable (find)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef

data Synth m = Synth {
  cachedDestination :: Node,
  spec :: SynthSpec m,
  nodes :: [Node]
  --audioBuffers :: Map Int AudioBuffer, -- TODO restarting a AudioBufferSourceNode needs to replace the buffers
  } deriving (Show)

playSynth_ :: AudioIO m => SynthDef m a -> m (Synth m)
playSynth_ x = execSynthDef x >>= synthSpecToSynth_

playSynth :: AudioIO m => Node -> SynthDef m a -> m (Synth m)
playSynth dest x = execSynthDef x >>= synthSpecToSynth dest

synthSpecToSynth_ :: AudioIO m => SynthSpec m -> m (Synth m)
synthSpecToSynth_ x = do
  dest <- createDestination
  synthSpecToSynth dest x

synthSpecToSynth :: AudioIO m => Node -> SynthSpec m -> m (Synth m)
synthSpecToSynth dest x = do
  ns <- sequence $ nodeBuilders x
  mapM_ (uncurry (makeConnections dest ns)) $ connections x
  disconnectOnStop ns
  return $ Synth { cachedDestination = dest, spec = x, nodes = ns }

makeConnections :: AudioIO m => Node -> [Node] -> NodeRef -> NodeRef -> m ()
makeConnections dest ns (NodeRef from) DestinationRef = connectNodes (ns!!from) dest
makeConnections _ ns (NodeRef from) (NodeRef to) = connectNodes (ns!!from) (ns!!to)
makeConnections _ ns (NodeRef from) (ParamRef to pType) = createParameter (ns!!to) (show pType) >>= connectNodes (ns!!from)
makeConnections _ _ _ _ = error "Malformed graph structure."

-- *** Note: there is probably a bug connected to the definition of disconnectOnStop below:
-- it attaches an onended callback to a single source node from a list of nodes, which doesn't
-- properly cover the case where there are multiple source nodes that end at different times.

disconnectOnStop :: (Foldable t, AudioIO m) => t Node -> m ()
disconnectOnStop ns = maybe (error "instantiating sourceless Synth") f $ find isSourceNode ns
  where f x = liftIO $ onended x $ \_ -> mapM_ disconnectAll ns

startSynth :: AudioIO m => Double -> Synth m -> m ()
startSynth t0 s = do
  mapM_ (startNode t0) $ nodes s
  maybe (return ()) (\t -> stopSynth (t0+t) s) $ deletionTime (spec s)
  mapM_ (scheduleChange (nodes s) t0) $ changes (spec s)

scheduleChange :: AudioIO m => [Node] -> Double -> Change -> m ()
scheduleChange ns t0 (SetValue (ParamRef i pType) v t) = void $ setValueAtTime (ns!!i) (show pType) v (t0+t)
scheduleChange ns t0 (LinearRampToValue (ParamRef i pType) v t) = void $ linearRampToValueAtTime (ns!!i) (show pType) v (t0+t)
scheduleChange ns t0 (ExponentialRampToValue (ParamRef i pType) v t) = void $ exponentialRampToValueAtTime (ns!!i) (show pType) v (t0+t)
scheduleChange ns t0 (CurveToValue (ParamRef i pType) curve t dur) = void $ setValueCurveAtTime (ns!!i) (show pType) curve (t0+t) dur
scheduleChange _ _ _ = error "scheduleChange targeted non-ParamRef node"

startSynthNow :: AudioIO m => Synth m -> m ()
startSynthNow s = do
  t <- audioTime
  startSynth (t + 0.050) s

stopSynth :: AudioIO m => Double -> Synth m -> m ()
stopSynth t s = mapM_ (stopNode t) $ nodes s

stopSynthNow :: AudioIO m => Synth m -> m ()
stopSynthNow s = do
  t <- audioTime
  stopSynth t s

restartSynth :: AudioIO m => Double -> Synth m -> m ()
restartSynth t s = do
  stopSynth t s
  s' <- synthSpecToSynth (cachedDestination s) (spec s)
  startSynth t s'
