{-# LANGUAGE JavaScriptFFI #-}

module Sound.MusicW.Worklets where

import GHCJS.Types
import GHCJS.Prim (toJSString)
import Control.Monad
import Control.Monad.IO.Class

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef
import Sound.MusicW.WorkletsJS (workletsJS)

-- note: for any of these functions to work one must have a secure browser context
-- AND one must have called (successfully) addWorklets :: AudioContext -> IO ()

-- worklets for unary functions from JavaScript Math library

absWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
absWorklet x = audioWorklet "abs-processor" [x]

acosWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
acosWorklet x = audioWorklet "acos-processor" [x]

acoshWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
acoshWorklet x = audioWorklet "acosh-processor" [x]

asinWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
asinWorklet x = audioWorklet "asin-processor" [x]

asinhWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
asinhWorklet x = audioWorklet "asinh-processor" [x]

atanWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
atanWorklet x = audioWorklet "atan-processor" [x]

atanhWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
atanhWorklet x = audioWorklet "atanh-processor" [x]

cbrtWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
cbrtWorklet x = audioWorklet "cbrt-processor" [x]

ceilWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
ceilWorklet x = audioWorklet "ceil-processor" [x]

cosWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
cosWorklet x = audioWorklet "cos-processor" [x]

coshWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
coshWorklet x = audioWorklet "cosh-processor" [x]

expWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
expWorklet x = audioWorklet "exp-processor" [x]

floorWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
floorWorklet x = audioWorklet "floor-processor" [x]

logWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
logWorklet x = audioWorklet "log-processor" [x]

log2Worklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
log2Worklet x = audioWorklet "log2-processor" [x]

log10Worklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
log10Worklet x = audioWorklet "log10-processor" [x]

roundWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
roundWorklet x = audioWorklet "round-processor" [x]

signWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
signWorklet x = audioWorklet "sign-processor" [x]

sinWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sinWorklet x = audioWorklet "sin-processor" [x]

sinhWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sinhWorklet x = audioWorklet "sinh-processor" [x]

sqrtWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sqrtWorklet x = audioWorklet "sqrt-processor" [x]

tanWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
tanWorklet x = audioWorklet "tan-processor" [x]

tanhWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
tanhWorklet x = audioWorklet "tanh-processor" [x]

truncWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
truncWorklet x = audioWorklet "trunc-processor" [x]

  
-- other worklets for unary functions

fractWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
fractWorklet x = audioWorklet "fract-processor" [x]

midiCpsWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
midiCpsWorklet x = audioWorklet "midiCps-processor" [x]

cpsMidiWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
cpsMidiWorklet x = audioWorklet "cpsMidi-processor" [x]

ampDbWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
ampDbWorklet x = audioWorklet "ampDb-processor" [x]

dbAmpWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
dbAmpWorklet x = audioWorklet "dbAmp-processor" [x]


-- other worklets

equalWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
equalWorklet in1 in2 = audioWorklet "equal-processor" [in1,in2]

notEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
notEqualWorklet in1 in2 = audioWorklet "notEqual-processor" [in1,in2]

greaterThanWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
greaterThanWorklet in1 in2 = audioWorklet "greaterThan-processor" [in1,in2]

greaterThanOrEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
greaterThanOrEqualWorklet in1 in2 = audioWorklet "greaterThanOrEqual-processor" [in1,in2]

lessThanWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
lessThanWorklet in1 in2 = audioWorklet "lessThan-processor" [in1,in2]

lessThanOrEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
lessThanOrEqualWorklet in1 in2 = audioWorklet "lessThanOrEqual-processor" [in1,in2]

safeDivideWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
safeDivideWorklet in1 in2 = audioWorklet "safeDivide-processor" [in1,in2]

unsafeDivideWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
unsafeDivideWorklet in1 in2 = audioWorklet "unsafeDivide-processor" [in1,in2]

powWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
powWorklet in1 in2 = audioWorklet "pow-processor" [in1,in2]

clipWorklet :: AudioIO m => NodeRef -> NodeRef -> NodeRef -> SynthDef m NodeRef
clipWorklet lo hi input = audioWorklet "clip-processor" [lo,hi,input]

maxWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
maxWorklet in1 in2 = audioWorklet "max-processor" [in1,in2]

minWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
minWorklet in1 in2 = audioWorklet "min-processor" [in1,in2]

whiteNoiseWorklet :: AudioIO m => SynthDef m NodeRef
whiteNoiseWorklet = audioWorklet "white-noise-processor" []

sinToSqrWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sinToSqrWorklet x = audioWorklet "sin-to-sqr-processor" [x]

sinToTriWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sinToTriWorklet x = audioWorklet "sin-to-tri-processor" [x]


-- | sinToSawWorklet is for producing an ideal (ie. not band-limited) sawtooth
-- wave (a phasor, in other words) from two phase-aligned sine waves. The first
-- sine wave argument should be at one quarter the desired frequency, and the second
-- sine wave argument should be at half the desired frequency.

sinToSawWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
sinToSawWorklet x y = audioWorklet "sin-to-saw-processor" [x,y]

-- | stepWorklet produces an output by selecting from a list of inputs on the basis
-- of another, "selector" input from from 0-1. The space of 0-1 is divided over however
-- many inputs are provided in the argument, and the output at any given sample only
-- comes from one of the inputs (there is no mixing or cross-fading between the inputs).

stepWorklet :: AudioIO m => [NodeRef] -> NodeRef -> SynthDef m NodeRef
stepWorklet xs y = audioWorklet "step-processor" (y:xs)

modWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
modWorklet x y = audioWorklet "mod-processor" [x,y]

audioWorklet :: AudioIO m => String -> [NodeRef] -> SynthDef m NodeRef
audioWorklet workletName inputs = do
  let iChnls = length inputs -- NOTE limiting assumption that each input NodeRef provides only one channel of input
  let oChnls = 1 -- NOTE limiting assumption that each audio worklet provides only one channel of output
  y <- addNodeBuilder (iChnls,oChnls) $ createAudioWorkletNode iChnls oChnls workletName
  zipWithM_ (\x n -> connect' x 0 y n) inputs [0..]
  return y

createAudioWorkletNode :: AudioIO m => Int -> Int -> String -> m Node
createAudioWorkletNode inChnls outChnls workletName = do
  ctx <- audioContext
  node <- liftIO $ js_createAudioWorkletNode ctx (toJSString workletName) inChnls outChnls
  _ <- setNodeField node "isSource" (outChnls > 0)
  _ <- setNodeField node "isSink" (inChnls > 0)
  setNodeField node "startable" False

foreign import javascript safe
  "new AudioWorkletNode($1, $2, { numberOfInputs: $3, numberOfOutputs: $4 } )"
  js_createAudioWorkletNode :: AudioContext -> JSVal -> Int -> Int -> IO Node

addWorklets :: AudioContext -> IO ()
addWorklets ac = do
  blob <- js_workletsBlob (toJSString workletsJS)
  url <- js_workletsURL blob
  js_audioWorkletAddModule ac url

foreign import javascript safe
  "new Blob([$1], { type: 'application/javascript' })"
  js_workletsBlob :: JSVal -> IO JSVal

foreign import javascript safe
  "URL.createObjectURL($1)"
  js_workletsURL :: JSVal -> IO JSVal

foreign import javascript safe
  "$1.audioWorklet.addModule($2);"
  js_audioWorkletAddModule :: AudioContext -> JSVal -> IO ()

