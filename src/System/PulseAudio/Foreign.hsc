{-# LANGUAGE
  CPP
, ForeignFunctionInterface
, GeneralizedNewtypeDeriving
, RecordWildCards
, ScopedTypeVariables
#-}

module System.PulseAudio.Foreign where

import Data.List (genericLength)
import Foreign
import Foreign.C.Types
import Foreign.C.String

import Debug.Trace

#include <pulse/pulseaudio.h>

-- Helpers --------------------------------------------------------------------

peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe s = if s == nullPtr then return Nothing
                                     else Just <$> peekCString s

-- Opaque types ---------------------------------------------------------------

data Context
data FormatInfo
data MainLoop
data MainLoopApi
data Operation
data Proplist
data SinkPortInfo
data SpawnApi
data SourcePortInfo
data ThreadedMainLoop

newtype Error = Error { getError :: CInt }

isError :: Error -> Bool
isError = (< 0) . getError

foreign import ccall "pa_context_connect" pa_context_connect
  :: Ptr Context -> CString -> ContextFlags -> Ptr SpawnApi -> IO Error

foreign import ccall "pa_context_disconnect" pa_context_disconnect
  :: Ptr Context -> IO ()

foreign import ccall "pa_context_errno" pa_context_errno
  :: Ptr Context -> IO Error

foreign import ccall "pa_context_get_sink_info_list" pa_context_get_sink_info_list
  :: Ptr Context -> FunPtr SinkInfoCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_get_sink_input_info_list" pa_context_get_sink_input_info_list
  :: Ptr Context -> FunPtr SinkInputInfoCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_get_source_info_list" pa_context_get_source_info_list
  :: Ptr Context -> FunPtr SourceInfoCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_get_source_output_info_list" pa_context_get_source_output_info_list
  :: Ptr Context -> FunPtr SourceOutputInfoCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_get_state" pa_context_get_state
  :: Ptr Context -> IO ContextState

foreign import ccall "pa_context_new" pa_context_new
  :: Ptr MainLoopApi -> CString -> IO (Ptr Context)

foreign import ccall "pa_context_set_subscribe_callback" pa_context_set_subscribe_callback
  :: Ptr Context -> FunPtr ContextSubscribeCb -> Ptr () -> IO ()

foreign import ccall "pa_context_set_sink_volume_by_index" pa_context_set_sink_volume_by_index
  :: Ptr Context -> SinkIndex -> Ptr CVolume -> FunPtr ContextSuccessCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_set_sink_input_volume" pa_context_set_sink_input_volume
  :: Ptr Context -> SinkInputIndex -> Ptr CVolume -> FunPtr ContextSuccessCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_set_source_volume_by_index" pa_context_set_source_volume_by_index
  :: Ptr Context -> SourceIndex -> Ptr CVolume -> FunPtr ContextSuccessCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_context_set_state_callback" pa_context_set_state_callback
  :: Ptr Context -> FunPtr ContextNotifyCb -> Ptr () -> IO ()

foreign import ccall "pa_context_subscribe" pa_context_subscribe
  :: Ptr Context -> SubscriptionMask -> FunPtr ContextSuccessCb -> Ptr () -> IO (Ptr Operation)

foreign import ccall "pa_mainloop_new" pa_mainloop_new
  :: IO (Ptr MainLoop)

foreign import ccall "pa_mainloop_quit" pa_mainloop_quit
  :: Ptr MainLoop -> Int -> IO ()

foreign import ccall "pa_mainloop_free" pa_mainloop_free
  :: Ptr MainLoop -> IO ()

foreign import ccall "&pa_mainloop_free" pa_mainloop_free_p
  :: FunPtr (Ptr MainLoop -> IO ())

foreign import ccall "pa_mainloop_get_api" pa_mainloop_get_api
  :: Ptr MainLoop -> IO (Ptr MainLoopApi)

foreign import ccall "pa_mainloop_iterate" pa_mainloop_iterate
  :: Ptr MainLoop -> Bool -> Ptr CInt -> IO ()

foreign import ccall "pa_mainloop_run" pa_mainloop_run
  :: Ptr MainLoop -> Ptr CInt -> IO ()

foreign import ccall "pa_operation_unref" pa_operation_unref
  :: Ptr Operation -> IO ()

foreign import ccall "pa_strerror" pa_strerror
  :: Error -> CString

foreign import ccall "pa_sw_volume_from_dB" pa_sw_volume_from_dB
  :: Double -> Volume

foreign import ccall "pa_sw_volume_to_dB" pa_sw_volume_to_dB
  :: Volume -> Double

foreign import ccall "pa_threaded_mainloop_accept" pa_threaded_mainloop_accept
  :: Ptr ThreadedMainLoop -> IO ()

foreign import ccall "pa_threaded_mainloop_get_api" pa_threaded_mainloop_get_api
  :: Ptr ThreadedMainLoop -> IO (Ptr MainLoopApi)

foreign import ccall "pa_threaded_mainloop_free" pa_threaded_mainloop_free
  :: Ptr ThreadedMainLoop -> IO ()

foreign import ccall "&pa_threaded_mainloop_free" pa_threaded_mainloop_free_p
  :: FunPtr (Ptr ThreadedMainLoop -> IO ())

foreign import ccall "pa_threaded_mainloop_lock" pa_threaded_mainloop_lock
  :: Ptr ThreadedMainLoop -> IO ()

foreign import ccall "pa_threaded_mainloop_new" pa_threaded_mainloop_new
  :: IO (Ptr ThreadedMainLoop)

foreign import ccall "pa_threaded_mainloop_signal" pa_threaded_mainloop_signal
  :: Ptr ThreadedMainLoop -> CInt -> IO ()

foreign import ccall "pa_threaded_mainloop_start" pa_threaded_mainloop_start
  :: Ptr ThreadedMainLoop -> IO CInt

foreign import ccall "pa_threaded_mainloop_stop" pa_threaded_mainloop_stop
  :: Ptr ThreadedMainLoop -> IO ()

foreign import ccall "pa_threaded_mainloop_unlock" pa_threaded_mainloop_unlock
  :: Ptr ThreadedMainLoop -> IO ()

foreign import ccall "pa_threaded_mainloop_wait" pa_threaded_mainloop_wait
  :: Ptr ThreadedMainLoop -> IO ()

type ContextNotifyCb = Ptr Context -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_context_notify_cb
  :: ContextNotifyCb -> IO (FunPtr ContextNotifyCb)

type ContextSubscribeCb = Ptr Context -> SubscriptionEventType -> CUInt -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_context_subscribe_cb
  :: ContextSubscribeCb -> IO (FunPtr ContextSubscribeCb)

type ContextSuccessCb = Ptr Context -> Int -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_context_success_cb
  :: ContextSuccessCb -> IO (FunPtr ContextSuccessCb)

type SinkInfoCb = Ptr Context -> Ptr SinkInfo -> Int -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_sink_info_cb
  :: SinkInfoCb -> IO (FunPtr SinkInfoCb)

type SinkInputInfoCb = Ptr Context -> Ptr SinkInputInfo -> Int -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_sink_input_info_cb
  :: SinkInputInfoCb -> IO (FunPtr SinkInputInfoCb)

type SourceInfoCb = Ptr Context -> Ptr SourceInfo -> Int -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_source_info_cb
  :: SourceInfoCb -> IO (FunPtr SourceInfoCb)

type SourceOutputInfoCb = Ptr Context -> Ptr SourceOutputInfo -> Int -> Ptr () -> IO ()
foreign import ccall "wrapper" pa_source_output_info_cb
  :: SourceOutputInfoCb -> IO (FunPtr SourceOutputInfoCb)

-- Newtypes -------------------------------------------------------------------

newtype SinkIndex = SinkIndex { getSinkIndex :: Word32 }
  deriving (Eq, Ord, Show, Storable)

instance Read SinkIndex where
  readsPrec p = fmap (fmap (\(a,b) -> (SinkIndex a, b))) (readsPrec p)

newtype SourceIndex = SourceIndex { getSourceIndex :: Word32 }
  deriving (Eq, Ord, Show, Storable)

instance Read SourceIndex where
  readsPrec p = fmap (fmap (\(a,b) -> (SourceIndex a, b))) (readsPrec p)

newtype SinkInputIndex = SinkInputIndex { getSinkInputIndex :: Word32 }
  deriving (Eq, Ord, Show, Storable)

instance Read SinkInputIndex where
  readsPrec p = fmap (fmap (\(a,b) -> (SinkInputIndex a, b))) (readsPrec p)

newtype SourceOutputIndex = SourceOutputIndex { getSourceOutputIndex :: Word32 }
  deriving (Eq, Ord, Show, Storable)

instance Read SourceOutputIndex where
  readsPrec p = fmap (fmap (\(a,b) -> (SourceOutputIndex a, b))) (readsPrec p)

-- Typedefs -------------------------------------------------------------------

-- pa_usec_t

type USec = Word64

-- pa_volume_t

type Volume = Word32

-- Enums ----------------------------------------------------------------------

-- pa_channel_position

newtype ChannelPosition = ChannelPosition { getChannelPosition :: CInt }
  deriving (Show, Storable)

#{enum ChannelPosition, ChannelPosition
  , channelPositionInvalid = PA_CHANNEL_POSITION_INVALID
  , channelPositionMono = PA_CHANNEL_POSITION_MONO
  , channelPositionFrontLeft = PA_CHANNEL_POSITION_FRONT_LEFT
  , channelPositionFrontRight = PA_CHANNEL_POSITION_FRONT_RIGHT
  , channelPositionFrontCenter = PA_CHANNEL_POSITION_FRONT_CENTER
  , channelPositionRearCenter = PA_CHANNEL_POSITION_REAR_CENTER
  , channelPositionRearLeft = PA_CHANNEL_POSITION_REAR_LEFT
  , channelPositionRearRight = PA_CHANNEL_POSITION_REAR_RIGHT
  , channelPositionLFE = PA_CHANNEL_POSITION_LFE
  , channelPositionFrontLeftOfCenter = PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
  , channelPositionFrontRightOfCenter = PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
  , channelPositionSideLeft = PA_CHANNEL_POSITION_SIDE_LEFT
  , channelPositionSideRight = PA_CHANNEL_POSITION_SIDE_RIGHT
  , channelPositionAux0 = PA_CHANNEL_POSITION_AUX0
  , channelPositionAux1 = PA_CHANNEL_POSITION_AUX1
  , channelPositionAux2 = PA_CHANNEL_POSITION_AUX2
  , channelPositionAux3 = PA_CHANNEL_POSITION_AUX3
  , channelPositionAux4 = PA_CHANNEL_POSITION_AUX4
  , channelPositionAux5 = PA_CHANNEL_POSITION_AUX5
  , channelPositionAux6 = PA_CHANNEL_POSITION_AUX6
  , channelPositionAux7 = PA_CHANNEL_POSITION_AUX7
  , channelPositionAux8 = PA_CHANNEL_POSITION_AUX8
  , channelPositionAux9 = PA_CHANNEL_POSITION_AUX9
  , channelPositionAux10 = PA_CHANNEL_POSITION_AUX10
  , channelPositionAux11 = PA_CHANNEL_POSITION_AUX11
  , channelPositionAux12 = PA_CHANNEL_POSITION_AUX12
  , channelPositionAux13 = PA_CHANNEL_POSITION_AUX13
  , channelPositionAux14 = PA_CHANNEL_POSITION_AUX14
  , channelPositionAux15 = PA_CHANNEL_POSITION_AUX15
  , channelPositionAux16 = PA_CHANNEL_POSITION_AUX16
  , channelPositionAux17 = PA_CHANNEL_POSITION_AUX17
  , channelPositionAux18 = PA_CHANNEL_POSITION_AUX18
  , channelPositionAux19 = PA_CHANNEL_POSITION_AUX19
  , channelPositionAux20 = PA_CHANNEL_POSITION_AUX20
  , channelPositionAux21 = PA_CHANNEL_POSITION_AUX21
  , channelPositionAux22 = PA_CHANNEL_POSITION_AUX22
  , channelPositionAux23 = PA_CHANNEL_POSITION_AUX23
  , channelPositionAux24 = PA_CHANNEL_POSITION_AUX24
  , channelPositionAux25 = PA_CHANNEL_POSITION_AUX25
  , channelPositionAux26 = PA_CHANNEL_POSITION_AUX26
  , channelPositionAux27 = PA_CHANNEL_POSITION_AUX27
  , channelPositionAux28 = PA_CHANNEL_POSITION_AUX28
  , channelPositionAux29 = PA_CHANNEL_POSITION_AUX29
  , channelPositionAux30 = PA_CHANNEL_POSITION_AUX30
  , channelPositionAux31 = PA_CHANNEL_POSITION_AUX31
  , channelPositionTopCenter = PA_CHANNEL_POSITION_TOP_CENTER
  , channelPositionTopFrontLeft = PA_CHANNEL_POSITION_TOP_FRONT_LEFT
  , channelPositionTopFrontRight = PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
  , channelPositionTopFrontCenter = PA_CHANNEL_POSITION_TOP_FRONT_CENTER
  , channelPositionTopRearLeft = PA_CHANNEL_POSITION_TOP_REAR_LEFT
  , channelPositionTopRearRight = PA_CHANNEL_POSITION_TOP_REAR_RIGHT
  , channelPositionTopRearCenter = PA_CHANNEL_POSITION_TOP_REAR_CENTER
  , channelPositionMax = PA_CHANNEL_POSITION_MAX
  }

-- pa_context_flags

newtype ContextFlags = ContextFlags { getContextFlags :: CInt }
  deriving (Show, Storable)

#{enum ContextFlags, ContextFlags
  , contextNoflags = PA_CONTEXT_NOFLAGS
  , contextNoautospawn = PA_CONTEXT_NOAUTOSPAWN
  , contextNofail = PA_CONTEXT_NOFAIL
}

-- pa_context_state

newtype ContextState = ContextState { getContextState :: CInt }
  deriving (Eq, Show, Storable)

#{enum ContextState, ContextState
  , contextUnconnected = PA_CONTEXT_UNCONNECTED
  , contextConnecting = PA_CONTEXT_CONNECTING
  , contextAuthorizing = PA_CONTEXT_AUTHORIZING
  , contextSettingName = PA_CONTEXT_SETTING_NAME
  , contextReady = PA_CONTEXT_READY
  , contextFailed = PA_CONTEXT_FAILED
  , contextTerminated = PA_CONTEXT_TERMINATED
  }

-- pa_sample_format

newtype SampleFormat = SampleFormat { getSampleFormat :: CInt }
  deriving (Show, Storable)

#{enum SampleFormat, SampleFormat
  , sampleU8 = PA_SAMPLE_U8
  , sampleALaw = PA_SAMPLE_ALAW
  , sampleULaw = PA_SAMPLE_ULAW
  , sampleS16LE = PA_SAMPLE_S16LE
  , sampleS16BE = PA_SAMPLE_S16BE
  , sampleFloat32LE = PA_SAMPLE_FLOAT32LE
  , sampleFloat32BE = PA_SAMPLE_FLOAT32BE
  , sampleS32LE = PA_SAMPLE_S32LE
  , sampleS32BE = PA_SAMPLE_S32BE
  , sampleS24LE = PA_SAMPLE_S24LE
  , sampleS24BE = PA_SAMPLE_S24BE 
  , sampleS2432LE = PA_SAMPLE_S24_32LE
  , sampleS2432BE = PA_SAMPLE_S24_32BE
  , sampleMax = PA_SAMPLE_MAX
  , sampleInvalid = PA_SAMPLE_INVALID
  }

-- pa_sink_flags

newtype SinkFlags = SinkFlags { getSinkFlags :: CInt }
  deriving (Show, Storable)

#{enum SinkFlags, SinkFlags
  , sinkNoflags = PA_SINK_NOFLAGS
  , sinkHwVolumeCtrl = PA_SINK_HW_VOLUME_CTRL
  , sinkLatency = PA_SINK_LATENCY
  , sinkHardware = PA_SINK_HARDWARE
  , sinkNetwork = PA_SINK_NETWORK
  , sinkHwMuteCtrl = PA_SINK_HW_MUTE_CTRL
  , sinkDecibelVolume = PA_SINK_DECIBEL_VOLUME
  , sinkFlatVolume = PA_SINK_FLAT_VOLUME
  , sinkDynamicLatency = PA_SINK_DYNAMIC_LATENCY
  , sinkSetFormats = PA_SINK_SET_FORMATS
  }

-- pa_sink_state

newtype SinkState = SinkState { getSinkState :: CInt }
  deriving (Show, Storable)

#{enum SinkState, SinkState
  , sinkInvalidState = PA_SINK_INVALID_STATE
  , sinkRunning = PA_SINK_RUNNING
  , sinkIdle = PA_SINK_IDLE
  , sinkSuspended = PA_SINK_SUSPENDED
  }

-- pa_source_flags

newtype SourceFlags = SourceFlags { getSourceFlags :: CInt }
  deriving (Show, Storable)

#{enum SourceFlags, SourceFlags
  , sourceNoflags = PA_SOURCE_NOFLAGS
  , sourceHwVolumeCtrl = PA_SOURCE_HW_VOLUME_CTRL
  , sourceLatency = PA_SOURCE_LATENCY
  , sourceHardware = PA_SOURCE_HARDWARE
  , sourceNetwork = PA_SOURCE_NETWORK
  , sourceHwMuteCtrl = PA_SOURCE_HW_MUTE_CTRL
  , sourceDecibelVolume = PA_SOURCE_DECIBEL_VOLUME
  , sourceDynamicLatency = PA_SOURCE_DYNAMIC_LATENCY
  , sourceFlatVolume = PA_SOURCE_FLAT_VOLUME
  }

-- pa_source_state

newtype SourceState = SourceState { getSourceState :: CInt }
  deriving (Show, Storable)

#{enum SourceState, SourceState
  , sourceInvalidState = PA_SOURCE_INVALID_STATE
  , sourceRunning = PA_SOURCE_RUNNING
  , sourceIdle = PA_SOURCE_IDLE
  , sourceSuspended = PA_SOURCE_SUSPENDED
  }

-- pa_subscription_event_type

newtype SubscriptionEventType = SubscriptionEventType { getSubscriptionEventType :: CInt }
  deriving (Show, Storable)

#{enum SubscriptionEventType, SubscriptionEventType
  , subscriptionEventSink = PA_SUBSCRIPTION_EVENT_SINK
  , subscriptionEventSource = PA_SUBSCRIPTION_EVENT_SOURCE
  , subscriptionEventSinkInput = PA_SUBSCRIPTION_EVENT_SINK_INPUT
  , subscriptionEventSourceOutput = PA_SUBSCRIPTION_EVENT_SOURCE_OUTPUT
  , subscriptionEventModule = PA_SUBSCRIPTION_EVENT_MODULE
  , subscriptionEventClient = PA_SUBSCRIPTION_EVENT_CLIENT
  , subscriptionEventSampleCache = PA_SUBSCRIPTION_EVENT_SAMPLE_CACHE
  , subscriptionEventServer = PA_SUBSCRIPTION_EVENT_SERVER
  , subscriptionEventCard = PA_SUBSCRIPTION_EVENT_CARD
  , subscriptionEventFacilityMask = PA_SUBSCRIPTION_EVENT_FACILITY_MASK
  , subscriptionEventNew = PA_SUBSCRIPTION_EVENT_NEW
  , subscriptionEventChange = PA_SUBSCRIPTION_EVENT_CHANGE
  , subscriptionEventRemove = PA_SUBSCRIPTION_EVENT_REMOVE
  , subscriptionEventTypeMask = PA_SUBSCRIPTION_EVENT_TYPE_MASK
  }

-- pa_subscription_mask

newtype SubscriptionMask = SubscriptionMask { getSubscriptionMask :: CInt }
  deriving (Show, Storable)

#{enum SubscriptionMask, SubscriptionMask
  , subscriptionMaskSink = PA_SUBSCRIPTION_MASK_SINK
  , subscriptionMaskSource = PA_SUBSCRIPTION_MASK_SOURCE
  , subscriptionMaskSinkInput = PA_SUBSCRIPTION_MASK_SINK_INPUT
  , subscriptionMaskSourceOutput = PA_SUBSCRIPTION_MASK_SOURCE_OUTPUT
  , subscriptionMaskModule = PA_SUBSCRIPTION_MASK_MODULE
  , subscriptionMaskClient = PA_SUBSCRIPTION_MASK_CLIENT
  , subscriptionMaskSampleCache = PA_SUBSCRIPTION_MASK_SAMPLE_CACHE
  , subscriptionMaskServer = PA_SUBSCRIPTION_MASK_SERVER
  , subscriptionMaskCard = PA_SUBSCRIPTION_MASK_CARD
  , subscriptionMaskAll = PA_SUBSCRIPTION_MASK_ALL
  }

-- Structures -----------------------------------------------------------------

-- pa_cvolume

data CVolume = CVolume
  { cvValues :: [Volume]
  } deriving (Show)

instance Storable CVolume where
  sizeOf _ = #{size struct pa_cvolume}
  alignment _ = #{alignment struct pa_cvolume}
  peek p = do
    size :: Word8 <- #{peek struct pa_cvolume, channels} p
    ints <- mapM (peekElemOff (#{ptr struct pa_cvolume, values} p) . fromIntegral) [0 .. size - 1]
    return $ CVolume ints
  poke p (CVolume vols) = do
    #{poke struct pa_cvolume, channels} p $ (genericLength vols :: Word8)
    let indexed = zip [0..] vols
    mapM_ (uncurry (pokeElemOff (#{ptr struct pa_cvolume, values} p))) indexed

-- pa_sink_info

data SinkInfo = SinkInfo
  { siName :: String
  , siIndex :: SinkIndex
  , siDescription :: String
  , siSampleSpec :: SampleSpec
  , siChannelMap :: ChannelMap
  , siOwnerModule :: Word32
  , siVolume :: CVolume
  , siMute :: Bool
  , siMonitorSource :: Word32
  , siMonitorSourceName :: String
  , siLatency :: USec
  , siDriver :: String
  , siFlags :: SinkFlags
  , siProplist :: Ptr Proplist
  , siConfiguredLatency :: USec
  , siBaseVolume :: Volume
  , siState :: SinkState
  , siNVolumeSteps :: Word32
  , siCard :: Word32
  , siPorts :: [Ptr SinkPortInfo]
  , siActivePort :: Ptr SinkPortInfo
  , siFormats :: [Ptr FormatInfo]
  } deriving (Show)

instance Storable SinkInfo where
  sizeOf _ = #{size struct pa_sink_info}
  alignment _ = #{alignment struct pa_sink_info}
  peek p = SinkInfo
    <$> (peekCString =<< #{peek struct pa_sink_info, name} p)
    <*> #{peek struct pa_sink_info, index} p
    <*> (peekCString =<< #{peek struct pa_sink_info, description} p)
    <*> #{peek struct pa_sink_info, sample_spec} p
    <*> #{peek struct pa_sink_info, channel_map} p
    <*> #{peek struct pa_sink_info, owner_module} p
    <*> #{peek struct pa_sink_info, volume} p
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_info, mute} p))
    <*> #{peek struct pa_sink_info, monitor_source} p
    <*> (peekCString =<< #{peek struct pa_sink_info, monitor_source_name} p)
    <*> #{peek struct pa_sink_info, latency} p
    <*> (peekCString =<< #{peek struct pa_sink_info, driver} p)
    <*> #{peek struct pa_sink_info, flags} p
    <*> #{peek struct pa_sink_info, proplist} p
    <*> #{peek struct pa_sink_info, configured_latency} p
    <*> #{peek struct pa_sink_info, base_volume} p
    <*> #{peek struct pa_sink_info, state} p
    <*> #{peek struct pa_sink_info, n_volume_steps} p
    <*> #{peek struct pa_sink_info, card} p
    <*> do
        size :: Word32 <- #{peek struct pa_sink_info, n_ports} p
        ptr <- #{peek struct pa_sink_info, ports} p
        if ptr == nullPtr
           then return []
           else mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
    <*> #{peek struct pa_sink_info, active_port} p
    <*> do
        size :: Word8 <- #{peek struct pa_sink_info, n_formats} p
        ptr :: Ptr (Ptr FormatInfo) <- #{peek struct pa_sink_info, formats} p
        mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
  poke _ (SinkInfo {..}) = error "PA: Currently no sinkinfo poke"

-- pa_sink_input_info

data SinkInputInfo = SinkInputInfo
  { siiIndex :: SinkInputIndex
  , siiName :: String
  , siiOwnerModule :: Word32
  , siiClient :: Word32
  , siiSink :: SinkIndex
  , siiSampleSpec :: SampleSpec
  , siiChannelMap :: ChannelMap
  , siiVolume :: CVolume
  , siiBufferUsec :: USec
  , siiSinkUsec :: USec
  , siiResampleMethod :: Maybe String
  , siiDriver :: String
  , siiMute :: Bool
  , siiProplist :: Ptr Proplist
  , siiCorked :: Bool
  , siiHasVolume :: Bool
  , siiVolumeWritable :: Bool
  , siiFormat :: Ptr FormatInfo
  } deriving (Show)

instance Storable SinkInputInfo where
  sizeOf _ = #{size struct pa_sink_input_info}
  alignment _ = #{alignment struct pa_sink_input_info}
  peek p = do
    hasVolume <- #{peek struct pa_sink_input_info, has_volume} p
    SinkInputInfo
      <$> #{peek struct pa_sink_input_info, index} p
      <*> (peekCString =<< #{peek struct pa_sink_input_info, name} p)
      <*> #{peek struct pa_sink_input_info, owner_module} p
      <*> #{peek struct pa_sink_input_info, client} p
      <*> #{peek struct pa_sink_input_info, sink} p
      <*> #{peek struct pa_sink_input_info, sample_spec} p
      <*> #{peek struct pa_sink_input_info, channel_map} p
      <*> (if hasVolume then #{peek struct pa_sink_input_info, volume} p else pure (CVolume []))
      <*> #{peek struct pa_sink_input_info, buffer_usec} p
      <*> #{peek struct pa_sink_input_info, sink_usec} p
      <*> (peekCStringMaybe =<< #{peek struct pa_sink_input_info, resample_method} p)
      <*> (peekCString =<< #{peek struct pa_sink_input_info, driver} p)
      <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_input_info, mute} p))
      <*> #{peek struct pa_sink_input_info, proplist} p
      <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_input_info, corked} p))
      <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_input_info, has_volume} p))
      <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_sink_input_info, volume_writable} p))
      <*> #{peek struct pa_sink_input_info, format} p
  poke _ (SinkInputInfo {..}) = error "PA: Currently no sink_input_info poke"

-- pa_source_info

data SourceInfo = SourceInfo
  { soName :: String
  , soIndex :: SourceIndex
  , soDescription :: String
  , soSampleSpec :: SampleSpec
  , soChannelMap :: ChannelMap
  , soOwnerModule :: Word32
  , soVolume :: CVolume
  , soMute :: Bool
  , soMonitorOfSink :: Word32
  , soMonitorOfSinkName :: Maybe String
  , soLatency :: USec
  , soDriver :: String
  , soFlags :: SourceFlags
  , soProplist :: Ptr Proplist
  , soConfiguredLatency :: USec
  , soBaseVolume :: Volume
  , soState :: SourceState
  , soNVolumeSteps :: Word32
  , soCard :: Word32
  , soPorts :: [Ptr SourcePortInfo]
  , soActivePort :: Ptr SourcePortInfo
  , soFormats :: [Ptr FormatInfo]
  } deriving (Show)

instance Storable SourceInfo where
  sizeOf _ = #{size struct pa_source_info}
  alignment _ = #{alignment struct pa_source_info}
  peek p = SourceInfo
    <$> (peekCString =<< #{peek struct pa_source_info, name} p)
    <*> #{peek struct pa_source_info, index} p
    <*> (peekCString =<< #{peek struct pa_source_info, description} p)
    <*> #{peek struct pa_source_info, sample_spec} p
    <*> #{peek struct pa_source_info, channel_map} p
    <*> #{peek struct pa_source_info, owner_module} p
    <*> #{peek struct pa_source_info, volume} p
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_source_info, mute} p))
    <*> #{peek struct pa_source_info, monitor_of_sink} p
    <*> (peekCStringMaybe =<< #{peek struct pa_source_info, monitor_of_sink_name} p)
    <*> #{peek struct pa_source_info, latency} p
    <*> (peekCString =<< #{peek struct pa_source_info, driver} p)
    <*> #{peek struct pa_source_info, flags} p
    <*> #{peek struct pa_source_info, proplist} p
    <*> #{peek struct pa_source_info, configured_latency} p
    <*> #{peek struct pa_source_info, base_volume} p
    <*> #{peek struct pa_source_info, state} p
    <*> #{peek struct pa_source_info, n_volume_steps} p
    <*> #{peek struct pa_source_info, card} p
    <*> do
        size :: Word32 <- #{peek struct pa_source_info, n_ports} p
        ptr <- #{peek struct pa_source_info, ports} p
        if ptr == nullPtr
           then return []
           else mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
    <*> #{peek struct pa_source_info, active_port} p
    <*> do
        size :: Word8 <- #{peek struct pa_source_info, n_formats} p
        ptr :: Ptr (Ptr FormatInfo) <- #{peek struct pa_source_info, formats} p
        mapM (peekElemOff ptr . fromIntegral) [0.. size - 1]
  poke _ (SourceInfo {..}) = error "PA: Currently no sourceinfo poke"

-- pa_source_output_info

data SourceOutputInfo = SourceOutputInfo
  { sooIndex :: SourceOutputIndex
  , sooName :: String
  , sooOwnerModule :: Word32
  , sooClient :: Word32
  , sooSource :: SourceIndex
  , sooSampleSpec :: SampleSpec
  , sooChannelMap :: ChannelMap
  , sooVolume :: CVolume
  , sooBufferUsec :: USec
  , sooSourceUsec :: USec
  , sooResampleMethod :: Maybe String
  , sooDriver :: String
  , sooMute :: Bool
  , sooProplist :: Ptr Proplist
  , sooCorked :: Bool
  , sooHasVolume :: Bool
  , sooVolumeWritable :: Bool
  , sooFormat :: Ptr FormatInfo
  } deriving (Show)

instance Storable SourceOutputInfo where
  sizeOf _ = #{size struct pa_source_output_info}
  alignment _ = #{alignment struct pa_source_output_info}
  peek p = SourceOutputInfo
    <$> #{peek struct pa_source_output_info, index} p
    <*> (peekCString =<< #{peek struct pa_source_output_info, name} p)
    <*> #{peek struct pa_source_output_info, owner_module} p
    <*> #{peek struct pa_source_output_info, client} p
    <*> #{peek struct pa_source_output_info, source} p
    <*> #{peek struct pa_source_output_info, sample_spec} p
    <*> #{peek struct pa_source_output_info, channel_map} p
    <*> #{peek struct pa_source_output_info, volume} p
    <*> #{peek struct pa_source_output_info, buffer_usec} p
    <*> #{peek struct pa_source_output_info, source_usec} p
    <*> (peekCStringMaybe =<< #{peek struct pa_source_output_info, resample_method} p)
    <*> (peekCString =<< #{peek struct pa_source_output_info, driver} p)
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_source_output_info, mute} p))
    <*> #{peek struct pa_source_output_info, proplist} p
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_source_output_info, corked} p))
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_source_output_info, has_volume} p))
    <*> ((/= (0 :: CInt)) <$> (#{peek struct pa_source_output_info, volume_writable} p))
    <*> #{peek struct pa_source_output_info, format} p
  poke _ (SourceOutputInfo {..}) = error "PA: Currently no source_output_info poke"

-- pa_sample_spec

data SampleSpec = SampleSpec
  { ssFormat :: SampleFormat
  , ssRate :: Word32
  , ssChannels :: Word8
  } deriving (Show)

instance Storable SampleSpec where
  sizeOf _ = #{size struct pa_sample_spec}
  alignment _ = #{alignment struct pa_sample_spec}
  peek p = SampleSpec
    <$> #{peek struct pa_sample_spec, format} p
    <*> #{peek struct pa_sample_spec, rate} p
    <*> #{peek struct pa_sample_spec, channels} p
  poke p (SampleSpec {..}) = do
    #{poke struct pa_sample_spec, format} p ssFormat
    #{poke struct pa_sample_spec, rate} p ssRate
    #{poke struct pa_sample_spec, channels} p ssChannels

-- pa_channel_map

data ChannelMap = ChannelMap
  { cmMap :: [ChannelPosition]
  } deriving (Show)

instance Storable ChannelMap where
  sizeOf _ = #{size struct pa_channel_map}
  alignment _ = #{alignment struct pa_channel_map}
  peek p = do
    size :: Word8 <- #{peek struct pa_channel_map, channels} p
    ints <- mapM (peekElemOff (#{ptr struct pa_channel_map, map} p) . fromIntegral ) [0..size - 1]
    return $ ChannelMap { cmMap = ints }
  poke p (ChannelMap pos) = do
    #{poke struct pa_channel_map, channels} p $ (genericLength pos :: Word8)
    let indexd = zip [0..] pos
    mapM_ (uncurry (pokeElemOff (#{ptr struct pa_channel_map, map} p))) indexd
