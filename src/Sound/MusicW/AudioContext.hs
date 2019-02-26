{-# LANGUAGE FlexibleInstances, JavaScriptFFI #-}

module Sound.MusicW.AudioContext where

import GHCJS.DOM.Types (js_eq)
import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Text
import Data.Time
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

newtype AudioContext = AudioContext JSVal

instance PToJSVal AudioContext where pToJSVal (AudioContext val) = val

instance PFromJSVal AudioContext where pFromJSVal = AudioContext

data AudioContextState = ACRunning | ACSuspended | ACClosed deriving (Show, Eq)

instance PToJSVal AudioContextState where
  pToJSVal ACRunning = js_acRunning
  pToJSVal ACSuspended = js_acSuspended
  pToJSVal ACClosed = js_acClosed

instance PFromJSVal AudioContextState where
  pFromJSVal x | x `js_eq` js_acRunning = ACRunning
  pFromJSVal x | x `js_eq` js_acSuspended = ACSuspended
  pFromJSVal x | x `js_eq` js_acClosed = ACClosed

foreign import javascript unsafe "\"running\"" js_acRunning :: JSVal
foreign import javascript unsafe "\"suspended\"" js_acSuspended :: JSVal
foreign import javascript unsafe "\"closed\"" js_acClosed :: JSVal

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

-- State management functions, both a sync and async version

foreign import javascript unsafe
  "$1.state"
  js_getState :: AudioContext -> IO JSVal

getState :: AudioContext -> IO AudioContextState
getState ac = pFromJSVal <$> js_getState ac

foreign import javascript interruptible
  "$1.resume().then($c)['catch'](function(e){$c(''+e);});" -- ffi parser treats 'catch' as keyword even in id ctx
  js_resumeSync :: AudioContext -> IO JSVal

foreign import javascript unsafe
  "$1.resume();"
  resume :: AudioContext -> IO ()

resumeSync :: AudioContext -> IO (Maybe Text)
resumeSync ac = pFromJSVal <$> js_resumeSync ac

foreign import javascript interruptible
  "$1.suspend().then($c)['catch'](function(e){$c(''+e);});" 
  js_suspendSync :: AudioContext -> IO JSVal

foreign import javascript unsafe
  "$1.suspend();"
  suspend :: AudioContext -> IO ()

suspendSync :: AudioContext -> IO (Maybe Text)
suspendSync ac = pFromJSVal <$> js_suspendSync ac

foreign import javascript interruptible
  "$1.close().then($c)['catch'](function(e){$c(''+e);});"
  js_closeSync :: AudioContext -> IO JSVal

foreign import javascript unsafe
  "$1.close();"
  close :: AudioContext -> IO ()

closeSync :: AudioContext -> IO (Maybe Text)
closeSync ac = pFromJSVal <$> js_closeSync ac

-- | Get the current audio time but cast it to a UTCTime for easier interoperation
-- with the standard Haskell time module. Note that the approach here might not work
-- if the audio context runs for more than one day.

getAudioUTCTime :: AudioContext -> IO UTCTime
getAudioUTCTime ctx = doubleToUTCTime <$> getAudioTime ctx

doubleToUTCTime :: Double -> UTCTime
doubleToUTCTime x = UTCTime {
  utctDay = toEnum 0,
  utctDayTime = realToFrac x -- *** this might not work if audio context runs for more than one day...
  }

utcTimeToDouble :: UTCTime -> Double
utcTimeToDouble x = max (realToFrac $ utctDayTime x) 0.0

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

liftAudioIO :: MonadIO m => AudioContextIO a -> m a
liftAudioIO x = do
  ac <- liftIO $ getGlobalAudioContext
  liftIO $ runAudioContextIO ac x

-- | Utility functions

dbamp :: Double -> Double
dbamp x = 10.0 ** (x / 20.0)

ampdb :: Double -> Double
ampdb x = 20.0 * (logBase 10 x)

midicps :: Double -> Double
midicps x = 440.0 * (2.0 ** ((x - 69.0) / 12.0))

cpsmidi :: Double -> Double
cpsmidi x = 69.0 + 12.0 * (logBase 2 (x / 440.0))
