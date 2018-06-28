# pulsecontrol - Query and control a pulseaudio server

## Example

    -- Connect to the default pulseaudio instance
    pa <- initPulse Nothing "pulsetest"

    -- Get a list of all sinks
    sinks <- runPulseM pa pulseListSinks
    -- Get a list of all sources
    sources <- runPulseM pa pulseListSources
    -- Get a list of all sink inputs
    sinkInputs <- runPulseM pa pulseListSinkInputs
    -- Get a list of all source outputs
    sourceOutputs <- runPulseM pa pulseListSourceOutputs

    -- Set the left and right channel of some sink input to 0 dB
    runPulseM pa $ pulseSetSinkInputVolume someSinkInputIndex [0.0, 0.0]
