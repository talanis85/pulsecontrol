{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.PulseAudio
  ( PulseAudio
  , execPulse
  , runPulse
  , pulseConnect
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
import Control.Monad.ContErr
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
  }

type PulseM = ContErrT String () (ReaderT PulseAudio IO)

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

  return PulseAudio
    { paContext = contextForeign
    , paMainLoop = mainloopForeign
    }

pulseQuit :: PulseAudio -> IO ()
pulseQuit pa =
  withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_threaded_mainloop_stop mainloop

execPulse :: PulseAudio -> (a -> IO ()) -> (String -> IO ()) -> PulseM a -> IO ()
execPulse pa onResult onError m = Control.Monad.void $
  runReaderT (runContErrT m (liftIO . onResult) (liftIO . onError)) pa

runPulse :: PulseAudio -> PulseM a -> IO (Either String a)
runPulse pa m = do
  result <- newEmptyMVar

  let onResult r = putMVar result (Right r)
      onError e = putMVar result (Left e)

  execPulse pa onResult onError m
  takeMVar result

{-
runPulse :: String -> (a -> IO ()) -> (String -> IO ()) -> PulseM a -> IO ()
runPulse name onResult onError m = do
  mainloop <- PA.pa_mainloop_new
  mainloopForeign <- newForeignPtr PA.pa_mainloop_free_p mainloop
  mainloopApi <- PA.pa_mainloop_get_api mainloop

  context <- withCString name (PA.pa_context_new mainloopApi)
  contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?

  let pa = PulseAudio
        { paContext = contextForeign
        , paMainLoop = mainloopForeign
        }

  r <- runReaderT (runContErrT m (liftIO . onResult) (liftIO . onError)) pa

  withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_mainloop_run mainloop nullPtr

runPulseSync :: String -> PulseM a -> IO (Either String a)
runPulseSync name m = do
  result <- newEmptyMVar

  mainloop <- PA.pa_mainloop_new
  mainloopForeign <- newForeignPtr PA.pa_mainloop_free_p mainloop
  mainloopApi <- PA.pa_mainloop_get_api mainloop

  context <- withCString name (PA.pa_context_new mainloopApi)
  contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?

  let pa = PulseAudio
        { paContext = contextForeign
        , paMainLoop = mainloopForeign
        }

  let onResult r = do
        putMVar result (Right r)
        PA.pa_mainloop_quit mainloop 0
      onError e = do
        putMVar result (Left e)
        PA.pa_mainloop_quit mainloop 0

  r <- runReaderT (runContErrT m (liftIO . onResult) (liftIO . onError)) pa

  withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_mainloop_run mainloop nullPtr

  takeMVar result
-}

getContextError :: Ptr PA.Context -> IO String
getContextError ctx = do
  err <- PA.pa_context_errno ctx
  peekCString (PA.pa_strerror err)

getStrError :: PA.Error -> IO String
getStrError err = peekCString (PA.pa_strerror err)

{-
-- pulseError :: String -> PulseM ()
pulseError :: String -> ReaderT PulseAudio IO ()
pulseError err = do
  pa <- ask
  liftIO $ do
    putStrLn err
    withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_mainloop_quit mainloop 0
-}

withPA :: PulseAudio -> (Ptr PA.ThreadedMainLoop -> Ptr PA.Context -> IO a) -> IO a
withPA pa f = withForeignPtr (paMainLoop pa) $ \mainloop ->
  withForeignPtr (paContext pa) $ \context -> do
    PA.pa_threaded_mainloop_lock mainloop
    r <- f mainloop context
    PA.pa_threaded_mainloop_unlock mainloop
    return r

pulseConnect :: Maybe String -> PulseM ()
pulseConnect server = contErrT $ \cont exit -> do
  liftIO $ traceIO "pulseConnect"
  pa <- ask
  err <- liftIO $ do
    callback <- PA.pa_context_notify_cb $ \ctx userdata -> do
      s <- PA.pa_context_get_state ctx
      print s
      let handler s | s == PA.contextReady = runReaderT (cont ()) pa
                    | s == PA.contextFailed = do
                        err <- getContextError ctx
                        withPA pa $ \mainloop context -> do
                          PA.pa_context_set_state_callback context nullFunPtr nullPtr
                        runReaderT (exit ("pa_context_connect_cb: " ++ err)) pa
                    | otherwise = return ()
      handler s
    withPA pa $ \mainloop context -> do
      PA.pa_context_set_state_callback context callback nullPtr
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
       exit ("pa_context_connect: " ++ errstr)
     else return ()

{-
pulseConnect :: String -> String -> IO (Either String PulseAudio)
pulseConnect name server = do
  withCString server $ \cServer -> do
    withCString name $ \cName -> do
      mainloop <- PA.pa_mainloop_new
      mainloopApi <- PA.pa_mainloop_get_api mainloop
      context <- PA.pa_context_new mainloopApi cName
      err <- PA.pa_context_connect context cServer PA.contextNoFlags nullPtr
      -- todo: error handling
      contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?
      mainloopForeign <- newForeignPtr PA.pa_mainloop_free_p mainloop
      return $ Right $ PulseAudio
        { paContext = contextForeign
        , paMainLoop = mainloopForeign
        }
-}

queryList mkCallback call = contErrT $ \cont exit -> do
  ref <- liftIO $ newIORef []
  pa <- ask
  liftIO $ do
    callback <- mkCallback $ \ctx x eol userdata -> do
      if eol > 0
         then do
           results <- readIORef ref
           runReaderT (cont results) pa
         else do
           x' <- peek x
           modifyIORef ref (x' :)
    withPA pa $ \mainloop context -> call context callback nullPtr
    return ()

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
pulseSetSinkVolume index dbVolumes = contErrT $ \cont exit -> do
  liftIO $ traceIO "pulseSetSinkVolume"
  pa <- ask
  liftIO $ do
    pCVolume <- malloc
    let cVolume = PA.CVolume (map dbVolumeToVolume dbVolumes)
    poke pCVolume cVolume
    callback <- PA.pa_context_success_cb $ \ctx success userdata -> do
      traceIO "pulseSetSinkVolume - callback"
      free pCVolume
      runReaderT (cont ()) pa
    withPA pa $ \mainloop context ->
      PA.pa_context_set_sink_volume_by_index context index pCVolume callback nullPtr
    return ()

mkSinkIndex :: Int -> PA.SinkIndex
mkSinkIndex = PA.SinkIndex . fromIntegral

mkSourceIndex :: Int -> PA.SourceIndex
mkSourceIndex = PA.SourceIndex . fromIntegral

pulseWaitForChange :: PulseM ()
pulseWaitForChange = contErrT $ \cont exit -> do
  liftIO $ traceIO "pulseWaitForChange"
  pa <- ask
  liftIO $ do
    callback <- PA.pa_context_subscribe_cb $ \ctx eventType idx userdata -> do
      withPA pa $ \mainloop context -> do
        PA.pa_context_set_subscribe_callback context nullFunPtr nullPtr
      runReaderT (cont ()) pa
    withPA pa $ \mainloop context -> do
      PA.pa_context_set_subscribe_callback context callback nullPtr
      PA.pa_context_subscribe context PA.subscriptionMaskAll nullFunPtr nullPtr
    return ()
