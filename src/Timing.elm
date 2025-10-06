module Timing exposing
    ( StepIdx
    , TimeConfig
    , getTotalSteps
    , noteDuration
    , validateStep
    )

-- Timing Configuration

import Utils exposing (justIf)


type alias TimeConfig =
    { bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    }


type alias StepIdx =
    Int



-- Timing Calculations


validateStep : StepIdx -> TimeConfig -> Maybe StepIdx
validateStep stepIdx tc =
    justIf (stepIdx < getTotalSteps tc) stepIdx


getTotalSteps : TimeConfig -> Int
getTotalSteps config =
    config.bars * config.beatsPerBar * config.subdivisions


noteDuration : TimeConfig -> Float
noteDuration config =
    -- 60 seconds/minute ÷ BPM = seconds per beat
    -- Then divide by subdivisions = seconds per step
    (60.0 / toFloat config.bpm) / toFloat config.subdivisions
