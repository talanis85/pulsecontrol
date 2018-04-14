module Main where

import Control.Concurrent
import Control.Monad.Trans
import System.PulseAudio

main :: IO ()
main = do
  pa <- pulseInit "pulsetest"
  result <- runPulse pa $ do
    pulseConnect (Just "/run/user/1000/pulse/native")
    sinks <- pulseListSinks
    sinkInputs <- pulseListSinkInputs
    sources <- pulseListSources
    sourceOutputs <- pulseListSourceOutputs
    pulseSetSinkVolume (mkSinkIndex 0) ([DbVolume 0, DbVolume 0])
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
  pulseQuit pa
