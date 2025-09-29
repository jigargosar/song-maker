module Model exposing
    ( Model
    , HistoryState
    , PlayState(..)
    , DrawState(..)
    , init
    , scaleConfig
    , timeConfig
    , pushToHistory
    , undo
    , redo
    )

import Browser.Navigation as Nav
import Grid exposing (PercGrid, PitchGrid)
import Instruments exposing (DrumKit, TonalInstrument)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Timing exposing (TimeConfig)
import Url exposing (Url)


-- Types


type PlayState
    = Stopped
    | PlayingStarted { startTime : Float }
    | Playing { startTime : Float, nextStep : Int }


type DrawState
    = NotDrawing
    | DrawingPitch
    | ErasingPitch
    | DrawingPerc
    | ErasingPerc


type alias HistoryState =
    { pitchGrid : PitchGrid
    , percGrid : PercGrid
    , scaleType : ScaleType
    , rootNote : RootNote
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }


type alias Model =
    { pitchGrid : PitchGrid
    , percGrid : PercGrid
    , scaleType : ScaleType
    , rootNote : RootNote
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    , currentTonalInstrument : TonalInstrument
    , currentDrumKit : DrumKit
    , drawState : DrawState
    , playState : PlayState
    , audioContextTime : Float
    , undoStack : List HistoryState
    , redoStack : List HistoryState
    , url : Url
    , key : Nav.Key
    }


-- Initialization


init : Url -> Nav.Key -> Model
init url key =
    { scaleType = Scales.major
    , rootNote = Scales.root
    , octaveStart = 3
    , octaveCount = 3
    , bars = 8
    , beatsPerBar = 4
    , subdivisions = 2
    , pitchGrid = Grid.emptyPitchGrid
    , percGrid = Grid.emptyPercGrid
    , drawState = NotDrawing
    , playState = Stopped
    , audioContextTime = 0.0
    , bpm = 120
    , currentTonalInstrument = Instruments.defaultTonalInstrument
    , currentDrumKit = Instruments.defaultDrumKit
    , undoStack = []
    , redoStack = []
    , url = url
    , key = key
    }


-- Config Helpers


scaleConfig : Model -> ScaleConfig
scaleConfig model =
    { scaleType = model.scaleType
    , rootNote = model.rootNote
    , octaveStart = model.octaveStart
    , octaveCount = model.octaveCount
    }


timeConfig : Model -> TimeConfig
timeConfig model =
    { bars = model.bars
    , beatsPerBar = model.beatsPerBar
    , subdivisions = model.subdivisions
    , bpm = model.bpm
    }


-- History Management


pushToHistory : Model -> Model
pushToHistory model =
    let
        currentHistoryState : HistoryState
        currentHistoryState =
            { pitchGrid = model.pitchGrid
            , percGrid = model.percGrid
            , scaleType = model.scaleType
            , rootNote = model.rootNote
            , octaveStart = model.octaveStart
            , octaveCount = model.octaveCount
            , bars = model.bars
            , beatsPerBar = model.beatsPerBar
            , subdivisions = model.subdivisions
            }

        newUndoStack =
            currentHistoryState :: model.undoStack

        newRedoStack =
            []
    in
    { model
        | undoStack = newUndoStack
        , redoStack = newRedoStack
    }


undo : Model -> Model
undo model =
    case model.undoStack of
        [] ->
            model

        lastState :: remainingUndoStack ->
            let
                newRedoStack : List HistoryState
                newRedoStack =
                    { pitchGrid = model.pitchGrid
                    , percGrid = model.percGrid
                    , scaleType = model.scaleType
                    , rootNote = model.rootNote
                    , octaveStart = model.octaveStart
                    , octaveCount = model.octaveCount
                    , bars = model.bars
                    , beatsPerBar = model.beatsPerBar
                    , subdivisions = model.subdivisions
                    }
                        :: model.redoStack

                newModel =
                    { model
                        | pitchGrid = lastState.pitchGrid
                        , percGrid = lastState.percGrid
                        , scaleType = lastState.scaleType
                        , rootNote = lastState.rootNote
                        , octaveStart = lastState.octaveStart
                        , octaveCount = lastState.octaveCount
                        , bars = lastState.bars
                        , beatsPerBar = lastState.beatsPerBar
                        , subdivisions = lastState.subdivisions
                        , undoStack = remainingUndoStack
                        , redoStack = newRedoStack
                    }
            in
            newModel


redo : Model -> Model
redo model =
    case model.redoStack of
        [] ->
            model

        lastState :: remainingRedoStack ->
            let
                newUndoStack =
                    { pitchGrid = model.pitchGrid
                    , percGrid = model.percGrid
                    , scaleType = model.scaleType
                    , rootNote = model.rootNote
                    , octaveStart = model.octaveStart
                    , octaveCount = model.octaveCount
                    , bars = model.bars
                    , beatsPerBar = model.beatsPerBar
                    , subdivisions = model.subdivisions
                    }
                        :: model.undoStack

                newModel =
                    { model
                        | pitchGrid = lastState.pitchGrid
                        , percGrid = lastState.percGrid
                        , scaleType = lastState.scaleType
                        , rootNote = lastState.rootNote
                        , octaveStart = model.octaveStart
                        , octaveCount = model.octaveCount
                        , bars = lastState.bars
                        , beatsPerBar = lastState.beatsPerBar
                        , subdivisions = lastState.subdivisions
                        , undoStack = newUndoStack
                        , redoStack = remainingRedoStack
                    }
            in
            newModel