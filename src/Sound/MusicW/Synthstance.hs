module Sound.MusicW.Synthstance (
  instantiateSynth,
  startSynth,
  startSynthNow,
  stopSynth,
  stopSynthNow
) where

import Sound.MusicW.Graph
import Sound.MusicW.Node
import Sound.MusicW.Spec
import Data.Foldable(find)
import Data.Map(Map,(!))

data Synthstance = Synthstance {
    synth :: Synth (),
    audioContext :: WebAudioContext,
    nodes :: Map Integer Node,
    --audioBuffers :: Map Int AudioBuffer, -- TODO restarting a AudioBufferSourceNode needs to replace the buffers
    nodeChanges :: [(Integer, Time -> Node -> IO ())],
    started :: Bool
  }

type Synstance = Synthstance

instantiateNode :: WebAudioContext -> NodeProps -> IO Node
instantiateNode ctx (SourceSpec x) = instantiateSourceNode x ctx
instantiateNode ctx (SourceSinkSpec x) = instantiateSourceSinkNode x ctx
instantiateNode ctx (SinkSpec x) = instantiateSinkNode x ctx

connectGraph :: Map Integer Node -> Graph -> IO ()
connectGraph m (Source (RefToNode _)) = return ()
connectGraph m (SourceSink (RefToNode to) from) = do
  connect (m!getNodeId from) $ m!to
  connectGraph m from
connectGraph m (Sink (RefToNode to) from) = do
  connect (m!getNodeId from) $ m!to
  connectGraph m from
connectGraph m (Sink (RefToParamOfNode to paramName) from) = do
  let param = audioParamNode (m!to) paramName
  connect (m!getNodeId from) param
  connectGraph m from
connectGraph _ _ = error "Malformed graph structure."

instantiateChange :: Change -> Time -> Node -> IO ()
instantiateChange (SetValue _ paramName val endTime) startTime node =
  setParamValueAtTime node paramName val $ startTime + endTime
instantiateChange (LinearRampToValue _ paramName val endTime) startTime node =
  linearRampToParamValueAtTime node paramName val $ startTime + endTime
instantiateChange (ExponentialRampToValue _ paramName val endTime) startTime node =
  exponentialRampToParamValueAtTime node paramName val $ startTime + endTime
instantiateChange (CurveToValue _ paramName curve curveStartTime duration) startTime node =
  setParamValueCurveAtTime node paramName curve (startTime + curveStartTime) duration

-- -- Attach an 'onended' callback to the first sourcenode
-- disconnectOnStop :: (Foldable t) => t Node -> IO ()
-- disconnectOnStop ns = let (Just src) = find isSourceNode ns in
--   onended src $ \_ -> mapM_ disconnectAll $ ns

disconnectOnStop ::(Foldable t) => t Node -> IO ()
disconnectOnStop ns = case (find isSourceNode ns) of
  (Just src) -> onended src $ \_ -> mapM_ disconnectAll $ ns
  (Nothing) -> do
    putStrLn "Warning: synthstance played with no source"
    return ()

instantiateSynth :: Synth a -> IO Synthstance
instantiateSynth x = do
  ctx <- globalAudioContext
  ns <- mapM (instantiateNode ctx) $ env x
  mapM_ (connectGraph ns) $ snd (graphs x)
  disconnectOnStop ns
  return $ Synthstance {
    synth = x >> return (),
    audioContext = ctx,
    nodes = ns,
    nodeChanges = fmap (\c -> (getNodeId $ node c, instantiateChange c)) (changes x),
    started = False
  }

-- Start the inst at the scheduled time. If the inst has a duration then it's stop
-- is also scheduled.
startSynth :: Time -> Synthstance -> IO Synthstance
startSynth time inst = do
  let ns = nodes inst
  if started inst then return () else do
    mapM_ (start time) ns
    case deletionTime $ synth inst of
      Just end -> stopSynth (time + end) inst
      Nothing -> return ()
  let cs = nodeChanges inst
  mapM_ (\(id, scheduleChange) -> scheduleChange time $ ns!id) cs
  return $ inst { started = True }

startSynthNow :: Synthstance -> IO Synthstance
startSynthNow inst = do
  time <- getCurrentTime $ audioContext inst
  startSynth (time + Millis 50) inst

stopSynth :: Time -> Synthstance -> IO ()
stopSynth time inst = mapM_ (stop time) $ nodes inst

stopSynthNow :: Synthstance -> IO ()
stopSynthNow inst = do
  time <- getCurrentTime $ audioContext inst
  stopSynth time inst

restartSynth :: Time -> Synthstance -> IO Synthstance
restartSynth time inst = do
  stopSynth time inst
  inst' <- instantiateSynth $ synth inst
  startSynth time inst'
