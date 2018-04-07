module System.PulseAudio
  ( PulseAudio
  , initPulse
  , runPulseM
  , runPulseMSync
  , runPulse
  , runPulseSync
  , pulseConnect
  , pulseQuit
  , pulseListSinks
  , pulseListSinkInputs
  , pulseListSources
  , pulseListSourceOutputs
  , DbVolume (..)
  , pulseSetSinkVolume
  , mkSinkIndex
  , mkSourceIndex
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
  , paMainLoop :: ForeignPtr PA.MainLoop
  }

type PulseM = ContErrT String () (ReaderT PulseAudio IO)

initPulse :: String -> IO PulseAudio
initPulse name = do
  mainloop <- PA.pa_mainloop_new
  mainloopForeign <- newForeignPtr PA.pa_mainloop_free_p mainloop
  mainloopApi <- PA.pa_mainloop_get_api mainloop

  context <- withCString name (PA.pa_context_new mainloopApi)
  contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?

  return PulseAudio
    { paContext = contextForeign
    , paMainLoop = mainloopForeign
    }

runPulseM :: PulseAudio -> (a -> IO ()) -> (String -> IO ()) -> PulseM a -> IO ()
runPulseM pa onResult onError m = Control.Monad.void $
  runReaderT (runContErrT m (liftIO . onResult) (liftIO . onError)) pa

runPulseMSync :: PulseAudio -> PulseM a -> IO (Either String a)
runPulseMSync pa m = do
  result <- newEmptyMVar

  let onResult r = do
        putMVar result (Right r)
        withForeignPtr (paMainLoop pa) $ \mainloop ->
          PA.pa_mainloop_quit mainloop 0
      onError e = do
        putMVar result (Left e)
        withForeignPtr (paMainLoop pa) $ \mainloop ->
          PA.pa_mainloop_quit mainloop 0

  runPulseM pa onResult onError m
  withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_mainloop_run mainloop nullPtr
  takeMVar result

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
                        withForeignPtr (paContext pa) $ \context ->
                          PA.pa_context_set_state_callback context nullFunPtr nullPtr
                        runReaderT (exit ("pa_context_connect_cb: " ++ err)) pa
                    | otherwise = return ()
      handler s
    withForeignPtr (paContext pa) $ \context -> do
      PA.pa_context_set_state_callback context callback nullPtr
      case server of
        Just server' -> withCString server' $ \cServer ->
          PA.pa_context_connect context cServer PA.contextNoflags nullPtr
        Nothing ->
          PA.pa_context_connect context nullPtr PA.contextNoflags nullPtr

  if PA.isError err
     then do
       errstr <- liftIO (getStrError err)
       liftIO $ withForeignPtr (paContext pa) $ \context ->
         PA.pa_context_set_state_callback context nullFunPtr nullPtr
       exit ("pa_context_connect: " ++ errstr)
     else return ()

pulseQuit :: PulseM ()
pulseQuit = do
  pa <- lift ask
  liftIO $ withForeignPtr (paMainLoop pa) $ \mainloop -> PA.pa_mainloop_quit mainloop 0

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
    withForeignPtr (paContext pa) $ \ctx -> call ctx callback nullPtr
    return ()

pulseListSinks :: PulseM [PA.SinkInfo]
pulseListSinks = queryList PA.pa_sink_info_cb PA.pa_context_get_sink_info_list

pulseListSinkInputs :: PulseM [PA.SinkInputInfo]
pulseListSinkInputs = queryList PA.pa_sink_input_info_cb PA.pa_context_get_sink_input_info_list

pulseListSources :: PulseM [PA.SourceInfo]
pulseListSources = queryList PA.pa_source_info_cb PA.pa_context_get_source_info_list

pulseListSourceOutputs :: PulseM [PA.SourceOutputInfo]
pulseListSourceOutputs = queryList PA.pa_source_output_info_cb PA.pa_context_get_source_output_info_list

newtype DbVolume = DbVolume { getDbVolume :: Double }

dbVolumeToVolume :: DbVolume -> PA.Volume
dbVolumeToVolume dbvol = PA.pa_sw_volume_from_dB (getDbVolume dbvol)

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
    withForeignPtr (paContext pa) $ \ctx ->
      PA.pa_context_set_sink_volume_by_index ctx index pCVolume callback nullPtr
    return ()

mkSinkIndex :: Int -> PA.SinkIndex
mkSinkIndex = PA.SinkIndex . fromIntegral

mkSourceIndex :: Int -> PA.SourceIndex
mkSourceIndex = PA.SourceIndex . fromIntegral
