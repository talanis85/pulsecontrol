{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.PulseAudio
  ( PulseAudio
  , PulseM
  , initPulse
  , runPulseM
  , pulseConnect
  , pulseListSinks
  , pulseListSinkInputs
  , pulseListSources
  , pulseListSourceOutputs
  , DbVolume (..)
  , pulseSetSinkVolume
  , pulseSetSinkInputVolume
  , mkSinkIndex
  , mkSourceIndex
  , SinkInfo (..)
  , SourceInfo (..)
  , SinkInputInfo (..)
  , SourceOutputInfo (..)
  , SinkIndex
  , SourceIndex
  , SinkInputIndex
  , SourceOutputIndex
  , defaultSinkIndex
  , defaultSourceIndex
  , defaultSinkInputIndex
  , defaultSourceOutputIndex
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.ContErr
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Time.Clock.POSIX
import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified System.PulseAudio.Foreign as PA
import System.PulseAudio.Foreign ( SinkIndex, SourceIndex, SinkInputIndex, SourceOutputIndex
                                 , defaultSinkIndex, defaultSourceIndex
                                 , defaultSinkInputIndex, defaultSourceOutputIndex )

import Debug.Trace

data PulseAudio = PulseAudio
  { paContext :: ForeignPtr PA.Context
  , paMainLoop :: ForeignPtr PA.MainLoop
  }

type PulseM a = ContErrT String () (ReaderT PulseAudio IO) a

initPulse :: String -> IO PulseAudio
initPulse name = do
  mainloop <- PA.pa_mainloop_new
  mainloopForeign <- newForeignPtr PA.pa_mainloop_free_p mainloop
  mainloopApi <- PA.pa_mainloop_get_api mainloop

  context <- withCString name (PA.pa_context_new mainloopApi)
  contextForeign <- newForeignPtr_ context -- todo: do we need a finalizer here?

  let pa = PulseAudio
        { paContext = contextForeign
        , paMainLoop = mainloopForeign
        }

  return pa

runPulseM :: PulseAudio -> PulseM a -> IO (Either String a)
runPulseM pa m = do
  result <- newEmptyMVar

  let onError e = liftIO $ putMVar result $ Left e
      onResult r = liftIO $ putMVar result $ Right r

  let timeout = 5
  time <- getPOSIXTime
  let loop = do
        time' <- getPOSIXTime
        if realToFrac (time' - time) > timeout
           then return (Left "timeout")
           else do
             withPA pa $ \mainloop context -> PA.pa_mainloop_iterate mainloop False nullPtr
             result' <- tryTakeMVar result
             case result' of
               Just r -> return r
               Nothing -> loop

  runReaderT (runContErrT m onResult onError) pa
  loop

data SinkInfo = SinkInfo
  { siIndex :: PA.SinkIndex
  , siMute :: Bool
  , siName :: String
  , siVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSinkInfo :: PA.SinkInfo -> SinkInfo
mkSinkInfo si = SinkInfo
  { siIndex = PA.siIndex si
  , siMute = PA.siMute si
  , siName = PA.siName si
  , siVolume = map volumeToDbVolume (PA.cvValues (PA.siVolume si))
  }

data SourceInfo = SourceInfo
  { soIndex :: PA.SourceIndex
  , soMute :: Bool
  , soName :: String
  , soVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSourceInfo :: PA.SourceInfo -> SourceInfo
mkSourceInfo so = SourceInfo
  { soIndex = PA.soIndex so
  , soMute = PA.soMute so
  , soName = PA.soName so
  , soVolume = map volumeToDbVolume (PA.cvValues (PA.soVolume so))
  }

data SinkInputInfo = SinkInputInfo
  { siiIndex :: PA.SinkInputIndex
  , siiMute :: Bool
  , siiName :: String
  , siiVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSinkInputInfo :: PA.SinkInputInfo -> SinkInputInfo
mkSinkInputInfo sii = SinkInputInfo
  { siiIndex = PA.siiIndex sii
  , siiMute = PA.siiMute sii
  , siiName = PA.siiName sii
  , siiVolume = map volumeToDbVolume (PA.cvValues (PA.siiVolume sii))
  }

data SourceOutputInfo = SourceOutputInfo
  { sooIndex :: PA.SourceOutputIndex
  , sooMute :: Bool
  , sooName :: String
  , sooVolume :: [DbVolume]
  } deriving (Show, Eq)

mkSourceOutputInfo :: PA.SourceOutputInfo -> SourceOutputInfo
mkSourceOutputInfo soo = SourceOutputInfo
  { sooIndex = PA.sooIndex soo
  , sooMute = PA.sooMute soo
  , sooName = PA.sooName soo
  , sooVolume = map volumeToDbVolume (PA.cvValues (PA.sooVolume soo))
  }

getContextError :: Ptr PA.Context -> IO String
getContextError ctx = do
  err <- PA.pa_context_errno ctx
  peekCString (PA.pa_strerror err)

getStrError :: PA.Error -> IO String
getStrError err = peekCString (PA.pa_strerror err)

withPA :: PulseAudio -> (Ptr PA.MainLoop -> Ptr PA.Context -> IO a) -> IO a
withPA pa f = withForeignPtr (paMainLoop pa) $ \mainloop ->
  withForeignPtr (paContext pa) $ \context ->
    f mainloop context

pulseConnect :: Maybe String -> PulseM ()
pulseConnect server = contErrT $ \cont exit -> do
  pa <- ask
  err <- liftIO $ do
    callback <- PA.pa_context_notify_cb $ \ctx userdata -> do
      let handler s | s == PA.contextReady = do
                        PA.pa_context_set_state_callback ctx nullFunPtr nullPtr
                        runReaderT (cont ()) pa
                    | s == PA.contextFailed = do
                        err <- liftIO $ getContextError ctx
                        PA.pa_context_set_state_callback ctx nullFunPtr nullPtr
                        runReaderT (exit ("pa_context_connect: " ++ err)) pa
                    | otherwise = return ()
      s <- liftIO $ PA.pa_context_get_state ctx
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

queryList mkCallback call = contErrT $ \cont exit -> do
  result <- liftIO $ newIORef []
  pa <- ask
  liftIO $ do
    callback <- mkCallback $ \ctx x eol userdata -> do
      if eol > 0
         then do
           r <- readIORef result
           runReaderT (cont r) pa
         else do
           x' <- peek x
           modifyIORef result (x' :)
    withPA pa $ \mainloop context -> do
      op <- call context callback (castPtr mainloop)
      PA.pa_operation_unref op

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

instance Read DbVolume where
  readsPrec p = fmap (fmap (\(a,b) -> (DbVolume a, b))) (readsPrec p)

instance Show DbVolume where
  show (DbVolume x) = show x ++ " dB"

dbVolumeToVolume :: DbVolume -> PA.Volume
dbVolumeToVolume dbvol = PA.pa_sw_volume_from_dB (getDbVolume dbvol)

volumeToDbVolume :: PA.Volume -> DbVolume
volumeToDbVolume vol = DbVolume (PA.pa_sw_volume_to_dB vol)

pulseSetSinkVolume :: PA.SinkIndex -> [DbVolume] -> PulseM ()
pulseSetSinkVolume index dbVolumes = contErrT $ \cont exit -> do
  pa <- ask
  liftIO $ do
    pCVolume <- malloc
    let cVolume = PA.CVolume (map dbVolumeToVolume dbVolumes)
    poke pCVolume cVolume
    callback <- PA.pa_context_success_cb $ \ctx success userdata -> do
      free pCVolume
      runReaderT (cont ()) pa
    withPA pa $ \mainloop context -> do
      op <- PA.pa_context_set_sink_volume_by_index context index pCVolume callback nullPtr
      PA.pa_operation_unref op
    return ()

pulseSetSinkInputVolume :: PA.SinkInputIndex -> [DbVolume] -> PulseM ()
pulseSetSinkInputVolume index dbVolumes = contErrT $ \cont exit -> do
  pa <- ask
  liftIO $ do
    pCVolume <- malloc
    let cVolume = PA.CVolume (map dbVolumeToVolume dbVolumes)
    poke pCVolume cVolume
    callback <- PA.pa_context_success_cb $ \ctx success userdata -> do
      free pCVolume
      runReaderT (cont ()) pa
    withPA pa $ \mainloop context -> do
      op <- PA.pa_context_set_sink_input_volume context index pCVolume callback (castPtr mainloop)
      PA.pa_operation_unref op
    return ()

mkSinkIndex :: Int -> PA.SinkIndex
mkSinkIndex = PA.SinkIndex . fromIntegral

mkSourceIndex :: Int -> PA.SourceIndex
mkSourceIndex = PA.SourceIndex . fromIntegral
