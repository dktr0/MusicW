module Sound.MusicW.Envelopes where

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef

ampEnv :: AudioIO m => AudioTime -> AudioTime -> Double -> AudioTime -> AudioTime -> NodeRef -> SynthDef m NodeRef
ampEnv a d s st r input = do
  g <- gain 0.0 input
  linearRampOnParam Gain 1.0 a g
  exponentialRampOnParam Gain s (a+d) g
  setParam Gain s (a+d+st) g
  linearRampOnParam Gain 0.0 (a+d+st+r) g

asr :: AudioIO m => AudioTime -> AudioTime -> AudioTime -> Double -> NodeRef -> SynthDef m NodeRef
asr a s r amp input = do
  g <- gain 0.0 input
  linearRampOnParam Gain amp a g
  setParam Gain amp (a+s) g
  linearRampOnParam Gain 0 (a+s+r) g

rectEnv :: AudioIO m => AudioTime -> AudioTime -> Double -> NodeRef -> SynthDef m NodeRef
rectEnv ar s amp = asr ar s ar amp

unitRectEnv :: AudioIO m => AudioTime-> AudioTime -> NodeRef -> SynthDef m NodeRef
unitRectEnv ar s = asr ar s ar 1
