module Sound.MusicW.Worklets where

import Sound.MusicW.AudioContext
import Sound.MusicW.SynthDef

-- note: for any of these functions to work one must have
-- a secure browser context AND one must have
-- called (successfully) audioWorkletAddModule :: AudioContext -> String -> IO ()
-- with the String a URL that points to the location of worklets.js

equalWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
equalWorklet in1 in2 = audioWorklet 1 1 "equal-processor"

notEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
notEqualWorklet in1 in2 = audioWorklet 1 1 "notEqual-processor"

greaterThanWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
greaterThanWorklet in1 in2 = audioWorklet 1 1 "greaterThan-processor"

greaterThanOrEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
greaterThanOrEqualWorklet in1 in2 = audioWorklet 1 1 "greaterThanOrEqual-processor"

lessThanWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
lessThanWorklet in1 in2 = audioWorklet 1 1 "lessThan-processor"

lessThanOrEqualWorklet :: AudioIO m => NodeRef -> NodeRef -> SynthDef m NodeRef
lessThanOrEqualWorklet in1 in2 = audioWorklet 1 1 "lessThanOrEqual-processor"
 