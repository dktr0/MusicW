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

powWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
powWorklet in1 in2 = audioWorklet "pow-processor" [in1,in2]

floorWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
floorWorklet x = audioWorklet "floor-processor" [x]

fractWorklet :: AudioIO m => NodeRef -> SynthDef m NodeRef
fractWorklet x = audioWorklet "fract-processor" [x]

clipWorklet :: AudioIO m => NodeRef -> NodeRef -> NodeRef -> SynthDef m NodeRef
clipWorklet lo hi input = audioWorklet "clip-processor" [lo,hi,input]

audioWorklet :: AudioIO m => String -> [NodeRef] -> SynthDef m NodeRef
audioWorklet workletName inputs = do
  let iChnls = length inputs -- NOTE limiting assumption that each input NodeRef provides only one channel of input
  let oChnls = 1 -- NOTE limiting assumption that each audio worklet provides only one channel of output
  y <- addNodeBuilder (iChnls,oChnls) $ createAudioWorkletNode iChnls oChnls workletName
  zipWithM (\x n -> connect' x 0 y n) inputs [0..]
  return y

createAudioWorkletNode :: AudioIO m => Int -> Int -> String -> m Node
createAudioWorkletNode inChnls outChnls workletName = do
  ctx <- audioContext
  node <- liftIO $ js_createAudioWorkletNode ctx (toJSString workletName) inChnls outChnls
  setNodeField node "isSource" (outChnls > 0)
  setNodeField node "isSink" (inChnls > 0)
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
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] == input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('equal-processor',EqualProcessor);\
\ \
\ class NotEqualProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] != input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('notEqual-processor',NotEqualProcessor);\
\ \
\ class GreaterThanProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] > input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('greaterThan-processor',GreaterThanProcessor);\
\ \
\ class GreaterThanOrEqualProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] >= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('greaterThanOrEqual-processor',GreaterThanOrEqualProcessor);\
\ \
\ class LessThanProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] < input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('lessThan-processor',LessThanProcessor);\
\ \
\ class LessThanOrEqualProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() {\
\    return [];\
\  }\
\  constructor() {\
\    super();\
\  }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input1[0][i] <= input2[0][i]) output[0][i] = 1; else output[0][i] = 0;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('lessThanOrEqual-processor',LessThanOrEqualProcessor);\
\ \
\ class MidiCpsProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = 440 * (2 ** ((input[0][i]-69)/12));\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('midiCps-processor',MidiCpsProcessor);\
\ \
\ class CpsMidiProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = 69 + (12 * (Math.log2(input[0][i]/440)));\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('cpsMidi-processor',CpsMidiProcessor);\
\ \
\ class DbAmpProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = 10 ** (input[0][i]/20);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('dbAmp-processor',DbAmpProcessor);\
\ \
\ class AmpDbProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = 20 * Math.log10(input[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('ampDb-processor',AmpDbProcessor);\
\ \
\ class AbsProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = Math.abs(input[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('abs-processor',AbsProcessor);\
\ \
\ class SqrtProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = Math.sqrt(input[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('sqrt-processor',SqrtProcessor);\
\ \
\ class SafeDivideProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      if(input2[0][i] == 0) output[0][i] = 0;\
\      else output[0][i] = input1[0][i] / input2[0][i];\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('safeDivide-processor',SafeDivideProcessor);\
\ \
\ class PowProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const input2 = inputs[1];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      output[0][i] = Math.pow(input1[0][i],input2[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('pow-processor',PowProcessor);\
\ \
\ class FloorProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      output[0][i] = Math.floor(input1[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('floor-processor',FloorProcessor);\
\ \
\ class FractProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const input1 = inputs[0];\
\    const output = outputs[0];\
\    for(let i = 0; i < input1[0].length; i++) {\
\      output[0][i] = input1[0][i] % 1;\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('fract-processor',FractProcessor);\
\ \
\ class ClipProcessor extends AudioWorkletProcessor {\
\  static get parameterDescriptors() { return []; }\
\  constructor() { super(); }\
\  process(inputs,outputs,parameters) {\
\    const min = inputs[0];\
\    const max = inputs[1];\
\    const input = inputs[2];\
\    const output = outputs[0];\
\    for(let i = 0; i < input[0].length; i++) {\
\      output[0][i] = Math.min(Math.max(input[0][i], min[0][i]), max[0][i]);\
\    }\
\    return true;\
\  }\
\ }\
\ registerProcessor('clip-processor',ClipProcessor);"
