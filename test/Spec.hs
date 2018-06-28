import Control.Monad.Trans
import System.Exit
import System.IO
import Sound.Pulse.Control

main :: IO ()
main = do
  pa <- initPulse Nothing "pulsetest"
  result <- runPulseM pa $ do
    liftIO $ putStrLn "pulseListSinks"
    sinks <- pulseListSinks
    liftIO $ putStrLn "pulseListSinkInputs"
    sinkInputs <- pulseListSinkInputs
    liftIO $ putStrLn "pulseListSources"
    sources <- pulseListSources
    liftIO $ putStrLn "pulseListSourceOutputs"
    sourceOutputs <- pulseListSourceOutputs
    return (sinks, sinkInputs, sources, sourceOutputs)
  case result of
    Left err -> hPutStr stderr ("Error: " ++ err ++ "\n") >> exitWith (ExitFailure 1)
    Right _ -> return ()
