module Sound.MusicW.Transformations where

import Control.Monad

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef
import Sound.MusicW.FloatArraySpec

clipAt :: AudioIO m => Double -> NodeRef -> SynthDef m NodeRef
clipAt x input = do
  let nSamples = 65536 :: Int
  let portion = (1.0 - x) / 2.0
  let nConstSamples = floor $ portion * fromIntegral nSamples
  let left = Const (nConstSamples-1) (-x) EmptyArray
  let mid = listToArraySpec [((2.0 * fromIntegral i) / (fromIntegral  nSamples)) - 1 | i <- [nConstSamples..(nSamples-nConstSamples)-1]]
  let right = Const (nConstSamples-1) x EmptyArray
  waveShaper (Right $ mconcat [left, mid, right]) NoOversampling input

-- | A panner not based on the WebAudio API panner node but rather on
-- straightforward equal power paining using two gain nodes.

equalPowerPan :: AudioIO m => Double -> NodeRef -> SynthDef m NodeRef
equalPowerPan x input = do
  left <- gain (gainFromDistance x) input
  right <- gain (gainFromDistance (1-x)) input
  channelMerger [left,right]

gainFromDistance :: Double -> Double
gainFromDistance x | abs x > 1 = 0
gainFromDistance x | otherwise = cos $ abs x * pi / 2

-- | circlePan pans a given single-channel signal around a circle of outputs
-- positions of 0 or 1 pan directly to the first output channel
-- positions between 0 and 1 move through the other outputs

circlePan :: AudioIO m => Int -> Double -> NodeRef -> SynthDef m NodeRef
circlePan 0 _ _ = constantSource 0
circlePan 1 _ input = return input
circlePan n pos input = do
  let pos' = pos - (fromIntegral $ floor pos)
  let pos'' = (fromIntegral n) * pos'
  let gs = fmap (gainFromDistance . ((-) pos'')) $ take n [0..]
  let g0 = gainFromDistance $ pos'' - (fromIntegral n)
  let gs' = (head gs + g0):tail gs -- taking account of "wrap-around"
  xs <- mapM (\x -> gain x input) gs'
  channelMerger xs

splay :: AudioIO m => Int -> [NodeRef] -> SynthDef m NodeRef
splay n [] = constantSource 0 >>= circlePan n 0
splay n inputs = do
  let i = 1/(fromIntegral $ length inputs - 1)
  let positions = fmap ((*) i) $ take (length inputs) [0..]
  xs <- zipWithM (circlePan n) positions inputs
  mix xs
