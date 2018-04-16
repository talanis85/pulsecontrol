{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.PulseAudio
  ( PulseAudio
  , PulseM
  , runPulseM
  , pulseConnect
  , waitForConnection
  , pulseInit
  , pulseQuit
  , pulseListSinks
  , pulseListSinkInputs
  , pulseListSources
  , pulseListSourceOutputs
  , DbVolume (..)
  , pulseSetSinkVolume
  , pulseWaitForChange
  , mkSinkIndex
  , mkSourceIndex
  , SinkInfo (..)
  , SourceInfo (..)
  , SinkInputInfo (..)
  , SourceOutputInfo (..)
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified System.PulseAudio.Foreign as PA

import Debug.Trace

data PulseAudio = PulseAudio
  { paContext :: ForeignPtr PA.Context
  , paMainLoop :: ForeignPtr PA.ThreadedMainLoop
  , paContextNotify :: MVar ()
  , paContextSubscribe :: MVar ()
  }

waitMVar :: MVar () -> IO ()
waitMVar mvar = do
  readMVar mvar
  tryTakeMVar mvar
  return ()

type PulseM = ExceptT String (ReaderT PulseAudio IO)

runPulseM :: PulseM a -> PulseAudio -> IO (Either String a)
runPulseM m pa = runReaderT (runExceptT m) pa

data SinkInfo = SinkInfo
  { siMute :: Bool
  , siName :: String
  , siVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSinkInfo :: PA.SinkInfo -> SinkInfo
mkSinkInfo si = SinkInfo
  { siMute = PA.siMute si
  , siName = PA.siName si
  , siVolume = map volumeToDbVolume (PA.cvValues (PA.siVolume si))
  }

data SourceInfo = SourceInfo
  { soMute :: Bool
  , soName :: String
  , soVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSourceInfo :: PA.SourceInfo -> SourceInfo
mkSourceInfo so = SourceInfo
  { soMute = PA.soMute so
  , soName = PA.soName so
  , soVolume = map volumeToDbVolume (PA.cvValues (PA.soVolume so))
  }

data SinkInputInfo = SinkInputInfo
  { siiMute :: Bool
  , siiName :: String
  , siiVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSinkInputInfo :: PA.SinkInputInfo -> SinkInputInfo
mkSinkInputInfo sii = SinkInputInfo
  { siiMute = PA.siiMute sii
  , siiName = PA.siiName sii
  , siiVolume = map volumeToDbVolume (PA.cvValues (PA.siiVolume sii))
  }

data SourceOutputInfo = SourceOutputInfo
  { sooMute :: Bool
  , sooName :: String
  , sooVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSourceOutputInfo :: PA.SourceOutputInfo -> SourceOutputInfo
mkSourceOutputInfo soo = SourceOutputInfo
  { sooMute = PA.sooMute soo
  , sooName = PA.sooName soo
  , sooVolume = map volumeToDbVolume (PA.cvValues (PA.sooVolume soo))
  }

pulseInit :: String -> IO PulseAudio
pulseInit name = do
  mainloop <- PA.pa_threaded_mainloop_new
  mainloopForeign <- newForeignPtr PA.pa_threaded_mainloop_free_p mainloop
  mainloopApi <- PA.pa_threaded_mainloop_get_api mainloop

  context <- withCString name (PA.pa_context_new mainloopApi)
  contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?

  PA.pa_threaded_mainloop_start mainloop

  contextNotifyMVar <- newEmptyMVar
  contextNotifyCb <- PA.pa_context_notify_cb $ \ctx userdata ->
    putMVar contextNotifyMVar ()
  PA.pa_context_set_state_callback context contextNotifyCb nullPtr

  contextSubscribeMVar <- newEmptyMVar
  contextSubscribeCb <- PA.pa_context_subscribe_cb $ \ctx eventType idx userdata ->
    putMVar contextSubscribeMVar ()
  PA.pa_context_set_subscribe_callback context contextSubscribeCb nullPtr
  PA.pa_context_subscribe context PA.subscriptionMaskAll nullFunPtr nullPtr

  return PulseAudio
    { paContext = contextForeign
    , paMainLoop = mainloopForeign
    , paContextNotify = contextNotifyMVar
    , paContextSubscribe = contextSubscribeMVar
    }

pulseQuit :: PulseAudio -> IO ()
pulseQuit pa =
  withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_threaded_mainloop_stop mainloop

getContextError :: Ptr PA.Context -> IO String
getContextError ctx = do
  err <- PA.pa_context_errno ctx
  peekCString (PA.pa_strerror err)

getStrError :: PA.Error -> IO String
getStrError err = peekCString (PA.pa_strerror err)

withPA :: PulseAudio -> (Ptr PA.ThreadedMainLoop -> Ptr PA.Context -> IO a) -> IO a
withPA pa f = withForeignPtr (paMainLoop pa) $ \mainloop ->
  withForeignPtr (paContext pa) $ \context -> do
    PA.pa_threaded_mainloop_lock mainloop
    r <- f mainloop context
    PA.pa_threaded_mainloop_unlock mainloop
    return r

pulseConnect :: Maybe String -> PulseM ()
pulseConnect server = do
  liftIO $ traceIO "pulseConnect"
  pa <- ask
  err <- liftIO $ do
    withPA pa $ \mainloop context -> do
      case server of
        Just server' -> withCString server' $ \cServer ->
          PA.pa_context_connect context cServer PA.contextNoflags nullPtr
        Nothing ->
          PA.pa_context_connect context nullPtr PA.contextNoflags nullPtr

  if PA.isError err
     then do
       errstr <- liftIO (getStrError err)
       liftIO $ withPA pa $ \mainloop context ->
         PA.pa_context_set_state_callback context nullFunPtr nullPtr
       throwError ("pa_context_connect: " ++ errstr)
     else return ()

waitForConnection :: PulseM ()
waitForConnection = do
  pa <- ask
  liftIO $ waitMVar (paContextNotify pa)
  s <- liftIO $ withContext pa $ \ctx -> PA.pa_context_get_state ctx
  liftIO $ print s
  let handler s | s == PA.contextReady = return ()
                | s == PA.contextFailed = do
                    err <- liftIO $ withContext pa $ \ctx -> getContextError ctx
                    throwError ("pa_context_connect_cb: " ++ err)
                | otherwise = waitForConnection
  handler s

waitForCompletion mainloop mvar = do
  r <- tryTakeMVar mvar
  case r of
    Nothing -> do
      PA.pa_threaded_mainloop_wait mainloop
      waitForCompletion mainloop mvar
    Just () -> return ()

queryList mkCallback call = do
  ref <- liftIO $ newIORef []
  done <- liftIO $ newEmptyMVar
  pa <- ask
  liftIO $ do
    callback <- mkCallback $ \ctx x eol userdata -> do
      if eol > 0
         then do
           let mainloop = castPtr userdata
           putMVar done ()
           PA.pa_threaded_mainloop_signal mainloop 1
         else do
           print x
           x' <- peek x
           modifyIORef ref (x' :)
    withMainLoop pa $ \mainloop ->
      withContext pa $ \context -> do
        PA.pa_threaded_mainloop_lock mainloop
        op <- call context callback (castPtr mainloop)
        waitForCompletion mainloop done
        PA.pa_operation_unref op
        PA.pa_threaded_mainloop_accept mainloop
        PA.pa_threaded_mainloop_unlock mainloop
    readIORef ref

withMainLoop :: PulseAudio -> (Ptr PA.ThreadedMainLoop -> IO a) -> IO a
withMainLoop pa f = withForeignPtr (paMainLoop pa) f

withContext :: PulseAudio -> (Ptr PA.Context -> IO a) -> IO a
withContext pa f = withForeignPtr (paContext pa) f

pulseListSinks :: PulseM [SinkInfo]
pulseListSinks = map mkSinkInfo <$> queryList PA.pa_sink_info_cb PA.pa_context_get_sink_info_list

pulseListSinkInputs :: PulseM [SinkInputInfo]
pulseListSinkInputs = map mkSinkInputInfo <$> queryList PA.pa_sink_input_info_cb PA.pa_context_get_sink_input_info_list

pulseListSources :: PulseM [SourceInfo]
pulseListSources = map mkSourceInfo <$> queryList PA.pa_source_info_cb PA.pa_context_get_source_info_list

pulseListSourceOutputs :: PulseM [SourceOutputInfo]
pulseListSourceOutputs = map mkSourceOutputInfo <$> queryList PA.pa_source_output_info_cb PA.pa_context_get_source_output_info_list

newtype DbVolume = DbVolume { getDbVolume :: Double }
  deriving (Ord, Eq, Num, Fractional, RealFrac, Real)

instance Show DbVolume where
  show (DbVolume x) = show x ++ " dB"

dbVolumeToVolume :: DbVolume -> PA.Volume
dbVolumeToVolume dbvol = PA.pa_sw_volume_from_dB (getDbVolume dbvol)

volumeToDbVolume :: PA.Volume -> DbVolume
volumeToDbVolume vol = DbVolume (PA.pa_sw_volume_to_dB vol)

pulseSetSinkVolume :: PA.SinkIndex -> [DbVolume] -> PulseM ()
pulseSetSinkVolume index dbVolumes = do
  liftIO $ traceIO "pulseSetSinkVolume"
  pa <- ask
  liftIO $ do
    pCVolume <- malloc
    let cVolume = PA.CVolume (map dbVolumeToVolume dbVolumes)
    poke pCVolume cVolume
    done <- newEmptyMVar
    callback <- PA.pa_context_success_cb $ \ctx success userdata -> do
      let mainloop = castPtr userdata
      traceIO "pulseSetSinkVolume - callback"
      free pCVolume
      putMVar done ()
      PA.pa_threaded_mainloop_signal mainloop 0
    withPA pa $ \mainloop context -> do
      op <- PA.pa_context_set_sink_volume_by_index context index pCVolume callback (castPtr mainloop)
      waitForCompletion mainloop done
      PA.pa_operation_unref op
    return ()

mkSinkIndex :: Int -> PA.SinkIndex
mkSinkIndex = PA.SinkIndex . fromIntegral

mkSourceIndex :: Int -> PA.SourceIndex
mkSourceIndex = PA.SourceIndex . fromIntegral

pulseWaitForChange :: PulseM ()
pulseWaitForChange = do
  pa <- ask
  liftIO $ waitMVar (paContextSubscribe pa)
