module Sound.MusicW.WorkletsJS where

unaryProcessor :: String -> String -> String -> String
unaryProcessor processorName className expr = part1 ++ part2 ++ part3 ++ part4 ++ part5
  where
    part1 = "class " ++ processorName ++ " extends AudioWorkletProcessor {\n"
    part2 = "constructor() { super(); this.notStarted = true; }\n\
\  process(inputs,outputs,parameters) {\n\
\    const input = inputs[0];\n\
\    const output = outputs[0];\n\
\    const blockSize = 128;\n\
\    const hasInput = !(input[0] === undefined);\n\
\    if(hasInput){\n\
\      this.notStarted = false;\n\
\      for(let n=0; n<blockSize;n++){\n"
    part3 = "output[0][n] = " ++ expr ++ ";\n"
    part4 = "}\n\
\    }\n\
\    return (this.notStarted || hasInput);\n\
\  }\n\
\ }\n"
    part5 = "registerProcessor('" ++ processorName ++ "'," ++ className ++ ");\n"
    
    
unaryProcessors :: String
unaryProcessors =
  -- unary functions from Javascript Math library, in alphabetical order
  unaryProcessor "abs-processor" "AbsProcessor" "Math.abs(input[0][n])" ++
  unaryProcessor "acos-processor" "AcosProcessor" "Math.acos(input[0][n])" ++
  unaryProcessor "acosh-processor" "AcoshProcessor" "Math.acosh(input[0][n])" ++
  unaryProcessor "asin-processor" "AsinProcessor" "Math.asin(input[0][n])" ++
  unaryProcessor "asinh-processor" "AsinhProcessor" "Math.asinh(input[0][n])" ++
  unaryProcessor "atan-processor" "AtanProcessor" "Math.atan(input[0][n])" ++
  unaryProcessor "atanh-processor" "AtanhProcessor" "Math.atanh(input[0][n])" ++
  unaryProcessor "cbrt-processor" "CbrtProcessor" "Math.cbrt(input[0][n])" ++
  unaryProcessor "ceil-processor" "CeilProcessor" "Math.ceil(input[0][n])" ++
  unaryProcessor "cos-processor" "CosProcessor" "Math.cos(input[0][n])" ++
  unaryProcessor "cosh-processor" "CoshProcessor" "Math.cosh(input[0][n])" ++
  unaryProcessor "exp-processor" "ExpProcessor" "Math.exp(input[0][n])" ++
  unaryProcessor "floor-processor" "FloorProcessor" "Math.floor(input[0][n])" ++
  unaryProcessor "log-processor" "LogProcessor" "Math.log(input[0][n])" ++
  unaryProcessor "log2-processor" "Log2Processor" "Math.log2(input[0][n])" ++
  unaryProcessor "log10-processor" "Log10Processor" "Math.log10(input[0][n])" ++
  unaryProcessor "round-processor" "RoundProcessor" "Math.round(input[0][n])" ++
  unaryProcessor "sign-processor" "SignProcessor" "Math.sign(input[0][n])" ++
  unaryProcessor "sin-processor" "SinProcessor" "Math.sin(input[0][n])" ++
  unaryProcessor "sinh-processor" "SinhProcessor" "Math.sinh(input[0][n])" ++
  unaryProcessor "sqrt-processor" "SqrtProcessor" "Math.sqrt(input[0][n])" ++
  unaryProcessor "tan-processor" "TanProcessor" "Math.tan(input[0][n])" ++
  unaryProcessor "tanh-processor" "TanhProcessor" "Math.tanh(input[0][n])" ++
  unaryProcessor "trunc-processor" "TruncProcessor" "Math.trunc(input[0][n])" ++
  -- other unary functions
  unaryProcessor "fract-processor" "FractProcessor" "input[0][n] % 1" ++
  unaryProcessor "midiCps-processor" "MidiCpsProcessor" "440 * (2 ** ((input[0][n]-69)/12))" ++
  unaryProcessor "cpsMidi-processor" "CpsMidiProcessor" "69 + (12 * (Math.log2(input[0][n]/440)))" ++
  unaryProcessor "ampDb-processor" "AmpDbProcessor" "20 * Math.log10(input[0][n])" ++
  unaryProcessor "dbAmp-processor" "DbAmpProcessor" "10 ** (input[0][n]/20)"

workletsJS :: String
workletsJS = unaryProcessors ++ otherProcessors 


otherProcessors :: String
otherProcessors = "\
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
\ registerProcessor('step-processor',StepProcessor);\
\ \
\ class ModProcessor extends AudioWorkletProcessor {\
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
\        output[0][i] = input1[0][i] % input2[0][i];\
\      }\
\    }\
\    return (this.notStarted || (hasInput1 && hasInput2));\
\  }\
\ }\
\ registerProcessor('mod-processor',ModProcessor);"

  
