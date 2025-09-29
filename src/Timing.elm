module Timing exposing
    ( TimeConfig
    , getTotalSteps
    , noteDuration
    )


-- Timing Configuration


type alias TimeConfig =
    { bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    }


-- Timing Calculations


getTotalSteps : TimeConfig -> Int
getTotalSteps config =
    config.bars * config.beatsPerBar * config.subdivisions


noteDuration : TimeConfig -> Float
noteDuration config =
    -- 60 seconds/minute ÷ BPM = seconds per beat
    -- Then divide by subdivisions = seconds per step
    (60.0 / toFloat config.bpm) / toFloat config.subdivisions