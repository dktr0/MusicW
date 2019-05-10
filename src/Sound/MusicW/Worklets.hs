module Sound.MusicW.Worklets where

import Sound.MusicW.AudioContext
import Sound.MusicW.SynthDef

-- note: for any of these functions to work one must have
-- a secure browser context AND one must have
-- called (successfully) audioWorkletAddModule :: AudioContext -> String -> IO ()
-- with the String a URL that points to the location of worklets.js

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
 