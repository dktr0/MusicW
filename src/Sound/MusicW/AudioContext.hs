{-# LANGUAGE FlexibleInstances, JavaScriptFFI #-}

module Sound.MusicW.AudioContext where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Time
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

newtype AudioContext = AudioContext JSVal

instance PToJSVal AudioContext where pToJSVal (AudioContext val) = val

instance PFromJSVal AudioContext where pFromJSVal = AudioContext

foreign import javascript safe
  "new (window.AudioContext || window.webkitAudioContext)()"
  newAudioContext :: IO AudioContext

foreign import javascript unsafe
  "window.___ac = $1;"
  setGlobalAudioContext :: AudioContext -> IO ()

foreign import javascript safe
  "if (window.___ac == null) { \
  \    window.___ac = new (window.AudioContext || window.webkitAudioContext)();\
  \} $r = window.___ac;"
  getGlobalAudioContext :: IO AudioContext

foreign import javascript unsafe
  "$1.currentTime"
  getAudioTime :: AudioContext -> IO Double

-- | Get the current audio time but cast it to a UTCTime for easier interoperation
-- with the standard Haskell time module. Note that the approach here might not work
-- if the audio context runs for more than one day.

getAudioUTCTime :: AudioContext -> IO UTCTime
getAudioUTCTime ctx = do
  x <- getAudioTime ctx
  return $ UTCTime {
    utctDay = toEnum 0,
    utctDayTime = realToFrac x -- *** this might not work if audio context runs for more than one day...
  }

foreign import javascript unsafe
  "$1.sampleRate"
  getSampleRate :: AudioContext -> IO Double

foreign import javascript unsafe
  "$1.destination"
  getDestination :: AudioContext -> IO JSVal

class (MonadIO m) => AudioIO m where
  audioContext :: m AudioContext

audioTime :: AudioIO m => m Double
audioTime = audioContext >>= liftIO . getAudioTime

audioUTCTime :: AudioIO m => m UTCTime
audioUTCTime = audioContext >>= liftIO . getAudioUTCTime

sampleRate :: AudioIO m => m Double
sampleRate = audioContext >>= liftIO . getSampleRate

destination :: AudioIO m => m JSVal -- TODO: not crazy about the JSVal return type here...
destination = audioContext >>= liftIO . getDestination

type AudioContextIO = ReaderT AudioContext IO

instance AudioIO AudioContextIO where
  audioContext = ask

runAudioContextIO :: AudioContext -> AudioContextIO a -> IO a
runAudioContextIO ac acio = (runReaderT acio) ac

-- | Utility functions

dbamp :: Double -> Double
dbamp x = 10.0 ** (x / 20.0)

ampdb :: Double -> Double
ampdb x = 20.0 * (logBase 10 x)

midicps :: Double -> Double
midicps x = 440.0 * (2.0 ** ((x - 69.0) / 12.0))

cpsmidi :: Double -> Double
cpsmidi x = 69.0 + 12.0 * (logBase 2 (x / 440.0))
