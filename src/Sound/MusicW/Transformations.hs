module Sound.MusicW.Transformations where

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
  let leftGain = cos $ x * pi / 2
  let rightGain = sin $ x * pi / 2
  left <- gain leftGain input
  right <- gain rightGain input
  channelMerger [left,right]
