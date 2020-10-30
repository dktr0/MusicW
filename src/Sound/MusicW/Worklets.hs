{-# LANGUAGE JavaScriptFFI #-}

module Sound.MusicW.Worklets where

import GHCJS.Types
import GHCJS.Prim (toJSString)
import Control.Monad
import Control.Monad.IO.Class

import Sound.MusicW.AudioContext
import Sound.MusicW.Node
import Sound.MusicW.SynthDef

-- note: for any of these functions to work one must have a secure browser context
-- AND one must have called (successfully) addWorklets :: AudioContext -> IO ()

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

midiCpsWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
midiCpsWorklet x = audioWorklet "midiCps-processor" [x]

cpsMidiWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
cpsMidiWorklet x = audioWorklet "cpsMidi-processor" [x]

ampDbWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
ampDbWorklet x = audioWorklet "ampDb-processor" [x]

dbAmpWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
dbAmpWorklet x = audioWorklet "dbAmp-processor" [x]

absWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
absWorklet x = audioWorklet "abs-processor" [x]

sqrtWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
sqrtWorklet x = audioWorklet "sqrt-processor" [x]

safeDivideWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
safeDivideWorklet in1 in2 = audioWorklet "safeDivide-processor" [in1,in2]

unsafeDivideWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
unsafeDivideWorklet in1 in2 = audioWorklet "unsafeDivide-processor" [in1,in2]

powWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
powWorklet in1 in2 = audioWorklet "pow-processor" [in1,in2]

floorWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
floorWorklet x = audioWorklet "floor-processor" [x]

ceilWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
ceilWorklet x = audioWorklet "ceil-processor" [x]

fractWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
fractWorklet x = audioWorklet "fract-processor" [x]

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

workletsJS :: String
workletsJS = "\
\ class EqualProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] == input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('equal-processor',EqualProcessor);\
\ \
\ class NotEqualProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] != input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('notEqual-processor',NotEqualProcessor);\
\ \
\ class GreaterThanProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] > input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('greaterThan-processor',GreaterThanProcessor);\
\ \
\ class GreaterThanOrEqualProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] >= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('greaterThanOrEqual-processor',GreaterThanOrEqualProcessor);\
\ \
\ class LessThanProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] < input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('lessThan-processor',LessThanProcessor);\
\ \
\ class LessThanOrEqualProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input1[0][i] <= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\      }\
\    }\
\    return (this.notStarted || ( hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('lessThanOrEqual-processor',LessThanOrEqualProcessor);\
\ \
\ class MidiCpsProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = 440 * (2 ** ((input[0][n]-69)/12));\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('midiCps-processor',MidiCpsProcessor);\
\ \
\ class CpsMidiProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = 69 + (12 * (Math.log2(input[0][n]/440)));\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('cpsMidi-processor',CpsMidiProcessor);\
\ \
\ class DbAmpProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = 10 ** (input[0][n]/20);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('dbAmp-processor',DbAmpProcessor);\
\ \
\ class AmpDbProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = 20 * Math.log10(input[0][n]);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('ampDb-processor',AmpDbProcessor);\
\ \
\ class AbsProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = Math.abs(input[0][n]);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('abs-processor',AbsProcessor);\
\ \
\ class SqrtProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = Math.sqrt(input[0][n]);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('sqrt-processor',SqrtProcessor);\
\ \
\ class SafeDivideProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        if(input2[0][i] == 0) output[0][i] = 0;\
\        else output[0][i] = input1[0][i] / input2[0][i];\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('safeDivide-processor',SafeDivideProcessor);\
\ \
\ class UnsafeDivideProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = input1[0][i] / input2[0][i];\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('unsafeDivide-processor',UnsafeDivideProcessor);\
\ \
\ class PowProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = Math.pow(input1[0][i],input2[0][i]);\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('pow-processor',PowProcessor);\
\ \
\ class FloorProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = Math.floor(input[0][n]);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('floor-processor',FloorProcessor);\
\ \
\ class CeilProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = Math.ceil(input[0][n]);\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('ceil-processor',CeilProcessor);\
\ \
\ class FractProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    const hasInput = !(input[0] === undefined);\
\    if(hasInput){\
\      this.notStarted = false;\
\      for(let n=0; n<blockSize;n++){\
\        output[0][n] = input[0][n] % 1;\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('fract-processor',FractProcessor);\
\ \
\ class ClipProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const min = inputs[0];\
\    const max = inputs[1];\
\    const input = inputs[2];\
\    const output = outputs[0];\
\    const hasMin = !(min[0] === undefined);\
\    const hasMax = !(max[0] === undefined);\
\    const hasInput = !(input[0] === undefined);\
\    const blockSize = 128;\
\    if(hasMin && hasMax && hasInput) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = Math.min(Math.max(input[0][i], min[0][i]), max[0][i]);\
\      }\
\    }\
\    return (this.notStarted || (hasMin && hasMax && hasInput));\
\  }\
\ }\
\ registerProcessor('clip-processor',ClipProcessor);\
\ \
\ class MaxProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = Math.max(input1[0][i],input2[0][i]);\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('max-processor',MaxProcessor);\
\ \
\ class MinProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = Math.min(input1[0][i],input2[0][i]);\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('min-processor',MinProcessor);\
\ \
\ class WhiteNoiseProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    for(let i = 0; i < blockSize; i++) {\
\      output[0][i] = Math.random()*2-1;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('white-noise-processor',WhiteNoiseProcessor);\
\ \
\ class SinToSqrProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const hasInput = !(input[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = (input[0][i] >= 0) ? 1 : -1;\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('sin-to-sqr-processor',SinToSqrProcessor);\
\ \
\ class SinToTriProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; this.MULT = 4/Math.PI; }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    const hasInput = !(input[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) {\
\        output[0][i] = Math.abs(Math.asin(input[0][i]))*this.MULT-1;\
\      }\
\    }\
\    return (this.notStarted || hasInput);\
\  }\
\ }\
\ registerProcessor('sin-to-tri-processor',SinToTriProcessor);\
\ \
\ class SinToSawProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; this.MULT = 4/Math.PI; }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    const hasInput1 = !(input1[0] === undefined);\
\    const hasInput2 = !(input2[0] === undefined);\
\    const blockSize = 128;\
\    if(hasInput1 && hasInput2) {\
\      this.notStarted = false;\
\      var a;\
\      for(let i = 0; i < blockSize; i++) {\
\        a = Math.abs(Math.asin(input1[0][i]))*this.MULT;\
\        output[0][i] = (input2[0][i] >= 0) ? (a-1) : (1-a);\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('sin-to-saw-processor',SinToSawProcessor);\
\ \
\ class StepProcessor extends AudioWorkletProcessor {\
\  constructor() { super(); this.notStarted = true; }\
\  process(inputs,outputs) {\
\    const output = outputs[0];\
\    const blockSize = 128;\
\    var inputCount=0;\
\    for(let i=0;i<inputs.length;i++){\
\      if(!(inputs[i][0] === undefined))inputCount++;\
\    }\
\    if(inputCount == 2 && (!(inputs[1][0] === undefined))) {\
\      this.notStarted = false;\
\      for(let i = 0; i < blockSize; i++) output[0][i] = inputs[1][0][i];\
\    }\
\    else if(inputs.length > 2 && inputs.length == inputCount) {\
\      this.notStarted = false;\
\      var a,n = inputs.length - 1;\
\      for(let i = 0; i < blockSize; i++) {\
\        a = inputs[0][0][i] * 0.5 + 0.5;\
\        a = Math.floor ((a - Math.trunc(a)) * n);\
\        a = (a >= 0) ? a : 0;\
\        a = (a < n) ? a : (n-1);\
\        output[0][i] = inputs[1+a][0][i];\
\      }\
\    }\
\    return (this.notStarted || (inputs.length == inputCount));\
\  }\
\ }\
\ registerProcessor('step-processor',StepProcessor);"
