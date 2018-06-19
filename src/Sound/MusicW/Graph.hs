{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.MusicW.Graph (
  Graph(..),
  Change(..),
  Synth(..),
  SynthBuilder,
  Reference(..),
  NodeProps(..),
  Env,
  buildSynth,
  buildSynth_,
  synthSource,
  synthSourceSink,
  synthSink,
  getNodeId,
  audioParamSink,
  setParamValue,
  linearRampToParamValue,
  exponentialRampToParamValue,
  curveToParamValue,
  setDeletionTime,
  maybeDelete
) where

import qualified Data.Map as Map

import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Sound.MusicW.Spec

-- See https://wiki.haskell.org/New_monads/MonadUnique for a simple monad transformer to support
-- generating unique values.

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

evalUniqueT :: Monad m => UniqueT m a -> m a
evalUniqueT (UniqueT s) = evalStateT s 0

type AudioParam = String

data Reference
  = RefToNode Integer
  | RefToParamOfNode Integer AudioParam
  deriving (Show)

data NodeProps
  = SourceSpec SourceNodeSpec
  | SourceSinkSpec SourceSinkNodeSpec
  | SinkSpec SinkNodeSpec
  deriving (Show)

type Env = Map.Map Integer NodeProps

data Graph
  = Source Reference
  | Sink Reference Graph
  | SourceSink Reference Graph
  | EmptyGraph
  deriving (Show)

data Change
  = SetValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | LinearRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | ExponentialRampToValue { node :: Graph, param :: AudioParam, value :: Double, endTime :: Time }
  | CurveToValue { node :: Graph, param :: AudioParam, values :: [Double], startTime :: Time, duration :: Time }
  deriving (Show)

data Synth a = Synth {
    graphs :: ([Graph], [Graph]),
    env :: Env,
    changes :: [Change],
    deletionTime :: Maybe Time,
    -- TODO preDelete :: Bool, delete what ever is already going on and start this one
    supplement :: a
  } deriving (Show)

type SynthBuilder = UniqueT Synth

buildSynth :: SynthBuilder a -> Synth a
buildSynth = evalUniqueT

buildSynth_ :: SynthBuilder a -> Synth ()
buildSynth_ = void . buildSynth

connectGraphs :: Graph -> Graph -> Graph
connectGraphs EmptyGraph y = y
connectGraphs x EmptyGraph = x
connectGraphs x@(Sink _ _) y = error $ "Sink can't be first: " ++ show x ++ " : " ++ show y
connectGraphs x y@(Source _) = error $ "Source can't be second: " ++ show x ++ " : " ++ show y
connectGraphs x (SourceSink r y) = SourceSink r (connectGraphs x y)
connectGraphs x (Sink r y) = Sink r (connectGraphs x y)

connectGraphs' :: ([Graph], [Graph]) -> Graph -> ([Graph], [Graph])
connectGraphs' ([], completed) y
  | isComplete y = ([], y:completed) -- Already completed
  | otherwise = ([y], completed) -- Incomplete, push to working stack
connectGraphs' (x@(Sink _ _):tl, completed) y@(Sink _ _)
  | isComplete y = (x:tl, y:completed)
  | otherwise = (y:x:tl, completed)
connectGraphs' (hd:tl, completed) y
  | isComplete y = (hd:tl, y:completed)
  | hasSource y  = (y:hd:tl, completed)
  | otherwise =
      if isComplete connected
        then (tl, connected:completed)
        else (connected:tl, completed)
      where connected = connectGraphs hd y

connectGraphStacks :: ([Graph], [Graph]) -> ([Graph], [Graph]) -> ([Graph], [Graph])
connectGraphStacks (incomp1, comp1) ([], comp2) = (incomp1, comp1 ++ comp2)
connectGraphStacks stacks (hd:tl, complete) =
  connectGraphStacks (connectGraphs' stacks hd) (tl, complete)

isComplete :: Graph -> Bool
isComplete (Sink _ x) = hasSource x
isComplete _ = False

getSource :: Graph -> Graph
getSource x@(Source _) = x
getSource (Sink _ x) = getSource x
getSource (SourceSink _ x) = getSource x
getSource EmptyGraph = EmptyGraph

hasSource :: Graph -> Bool
hasSource g = case getSource g of
  Source _ -> True
  _ -> False

instance Functor Synth where
  fmap f x = x { supplement = f (supplement x) }

instance Applicative Synth where
  pure x = Synth { graphs = ([], []), env = Map.empty, changes = [], deletionTime = Nothing, supplement = x }
  (Synth g1 e1 cs1 d1 f) <*> (Synth g2 e2 cs2 d2 x) = Synth {
    graphs = connectGraphStacks g1 g2,
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    deletionTime = combineDeletionTimes d1 d2,
    supplement = f x
  }

instance Monad Synth where
  (Synth g1 e1 cs1 d1 a) >>= f = Synth {
    graphs = connectGraphStacks g1 g2,
    env = Map.union e1 e2,
    changes = cs1 ++ cs2,
    deletionTime = combineDeletionTimes d1 d2,
    supplement = b
  } where (Synth g2 e2 cs2 d2 b) = f a

combineDeletionTimes :: Maybe Time -> Maybe Time -> Maybe Time
combineDeletionTimes Nothing Nothing = Nothing
combineDeletionTimes (Just t) Nothing = Just t
combineDeletionTimes Nothing (Just t) = Just t
combineDeletionTimes (Just t1) (Just t2) = Just (max t1 t2)

synthSource :: SourceNodeSpec -> SynthBuilder Graph
synthSource spec = do
  r <- fresh
  lift $ makeSynth r (Source (RefToNode r)) (SourceSpec spec)

synthSourceSink :: SourceSinkNodeSpec -> SynthBuilder Graph
synthSourceSink spec = do
  r <- fresh
  lift $ makeSynth r (SourceSink (RefToNode r) EmptyGraph) (SourceSinkSpec spec)

synthSink :: SinkNodeSpec -> SynthBuilder Graph
synthSink spec = do
  r <- fresh
  lift $ makeSynth r (Sink (RefToNode r) EmptyGraph) (SinkSpec spec)

makeSynth :: Integer -> Graph -> NodeProps -> Synth Graph
makeSynth r g np = Synth {
  graphs = ([g],[]),
  env = Map.singleton r np,
  changes = [],
  deletionTime = Nothing,
  supplement = g
}

audioParamSink :: AudioParam -> Graph -> SynthBuilder Graph
audioParamSink p g = lift $ Synth {
    graphs = ([g'],[]),
    env = Map.empty,
    changes = [],
    deletionTime = Nothing,
    supplement = g
  }
  where g' = Sink (RefToParamOfNode (getNodeId g) p) EmptyGraph

getNodeId :: Graph -> Integer
getNodeId (Source (RefToNode r)) = r
getNodeId (Sink (RefToNode r) _) = r
getNodeId (SourceSink (RefToNode r) _) = r
getNodeId _ = error "you used the f function incorrectly"

change :: a -> Change -> SynthBuilder a
change a c = lift $ Synth {
  graphs = ([], []),
  env = Map.empty,
  changes = [c],
  deletionTime = Nothing,
  supplement = a
}

setParamValue :: AudioParam -> Double -> Time -> Graph -> SynthBuilder Graph
setParamValue p v e g = change g $ SetValue g p v e

linearRampToParamValue :: AudioParam -> Double -> Time -> Graph -> SynthBuilder Graph
linearRampToParamValue p v e g = change g $ LinearRampToValue g p v e

exponentialRampToParamValue :: AudioParam -> Double -> Time -> Graph -> SynthBuilder Graph
exponentialRampToParamValue p v e g = change g $ ExponentialRampToValue g p v e

curveToParamValue :: AudioParam -> [Double] -> Time -> Time -> Graph -> SynthBuilder Graph
curveToParamValue p vs s d g = change g $ CurveToValue g p vs s d

setDeletionTime :: Time -> SynthBuilder ()
setDeletionTime t = lift $ Synth {
    graphs = ([], []),
    env = Map.empty,
    changes = [],
    deletionTime = Just t,
    supplement = ()
  }

maybeDelete :: Maybe Time -> SynthBuilder () -- move to where setDeletionTime
maybeDelete = maybe (return ()) (setDeletionTime)


test1 :: Synth ()
test1 = buildSynth $ do
  r <- synthSource $ Oscillator Sine (Hz 440)
  linearRampToParamValue "freq" 880 (Sec 4.0) r
  synthSource $ Oscillator Sine (Hz 220)
  synthSink Destination
  synthSink Destination
  synthSource $ Oscillator Sawtooth (Hz 110)
  synthSink Destination
  setDeletionTime (Sec 2.0)
  return ()

test2 :: Synth ()
test2 = buildSynth $ do
  _ <- synthSource $ Oscillator Sine (Hz 440)
  return ()
