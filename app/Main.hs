module Main where

import Control.Concurrent
import Control.Monad.Trans
import System.PulseAudio
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  pa <- initPulse (Just "/run/user/1000/pulse/native") "pulsetest"
  result <- runPulseM pa $ do
    pulseState >>= liftIO . print
    liftIO $ getLine
    pulseState >>= liftIO . print

    case args of
      [index, volume] -> pulseSetSinkInputVolume (read index) [read volume, read volume]
      _ -> return ()

    sinks <- pulseListSinks
    -- let sinks = [] :: [SinkInfo]
    sinkInputs <- pulseListSinkInputs
    -- let sinkInputs = [] :: [SinkInputInfo]
    sources <- pulseListSources
    -- let sources = [] :: [SourceInfo]
    sourceOutputs <- pulseListSourceOutputs
    -- let sourceOutputs = [] :: [SourceOutputInfo]
    -- pulseSetSinkVolume (mkSinkIndex 0) ([DbVolume 0, DbVolume 0])
    return (sinks, sinkInputs, sources, sourceOutputs)
  {-
  result <- runPulseSync "pulsetest" $ do
    pulseConnect (Just "/run/user/1000/pulse/native")
    sinks <- pulseListSinks
    sinkInputs <- pulseListSinkInputs
    sources <- pulseListSources
    sourceOutputs <- pulseListSourceOutputs
    pulseSetSinkVolume (mkSinkIndex 0) ([DbVolume 0, DbVolume 0])
    return (sinks, sinkInputs, sources, sourceOutputs)
  -}
  case result of
    Left err -> putStrLn ("Error: " ++ err)
    Right (sinks, sinkInputs, sources, sourceOutputs) -> do
      putStrLn "Sinks:"
      print sinks
      putStrLn "Sink Inputs:"
      print sinkInputs
      putStrLn "Sources:"
      print sources
      putStrLn "Source Outputs:"
      print sourceOutputs

  result <- runPulseM pa $ do
    pulseState >>= liftIO . print
    r <- pulseListSinks
    pulseState >>= liftIO . print
    return r

  case result of
    Left err -> putStrLn ("Error: " ++ err)
    Right sinks -> do
      putStrLn "Sinks:"
      print sinks
