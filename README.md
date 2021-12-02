# MusicW

Sound synthesis library, to be used with GHCJS and Web Audio API  

# History

- 0.3.9: added maxChannelCount, channelCount, setChannelCount
- 0.3.8: bug fix to disconnectNodes from Sound.MusicW.Node
- 0.3.7: added externalNode (allows pre-existing Web Audio node to be included in a SynthDef, is not disconnected when Synth ends)
- 0.3.6: added ceilWorklet
- 0.3.5.2: audio worklets reworked to address resource (CPU) leak issues
- 0.3.5.1: added input sanity checking to existing audio worklets (output 0 when inputs not connected yet)
- 0.3.5: added maxWorklet, minWorklet, whiteNoiseWorklet, sinToSqrWorklet, sinToTriWorklet, sinToSawWorklet, stepWorklet
- 0.3.4: added unsafeDivideWorklet, getGlobalAudioContextInteractive/Playback
- 0.3.3: added microphone
- 0.3.2: fixes to channelSplitter
- 0.3.1: added floorWorklet, fractWorklet, clipWorklet
- 0.3.0: support for analyser nodes
- 0.2.0: initial Hackage release
