module Model exposing (..)

import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Timing exposing (TimeConfig)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Url.Query.Pipeline as Pipeline
import Utils exposing (..)


type alias Flags =
    {}


init : Flags -> Url -> Nav.Key -> Model
init _ url key =
    let
        initialModel : Model
        initialModel =
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
    in
    initialModel
        |> applyQueryParams url
        |> applySong twinkleSong


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



-- Helper functions to create Grid configs from Model


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


type alias QueryParams =
    { bpm : Maybe Int
    , octaveStart : Maybe Int
    , octaveCount : Maybe Int
    }


queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Pipeline.succeed QueryParams
        |> Pipeline.optional (Query.int "bpm")
        |> Pipeline.optional (Query.int "octaveStart")
        |> Pipeline.optional (Query.int "octaveCount")


buildQuery : Model -> String
buildQuery model =
    UB.absolute
        []
        [ UB.int "bpm" model.bpm
        , UB.int "octaveStart" model.octaveStart
        , UB.int "octaveCount" model.octaveCount
        ]


parseQueryParams : Url -> Maybe QueryParams
parseQueryParams url =
    if url.query == Nothing then
        -- This check is required since when there is no query params we need to apply default params
        Nothing

    else
        Parser.parse (Parser.top <?> queryParser) url
            |> Maybe.andThen identity


applyQueryDefaults : Model -> Model
applyQueryDefaults model =
    { model
        | bpm = 120
        , octaveStart = 3
        , octaveCount = 3
    }


applyQueryParams : Url -> Model -> Model
applyQueryParams url model =
    case parseQueryParams url of
        Just params ->
            { model
                | bpm = Maybe.withDefault model.bpm params.bpm
                , octaveStart = Maybe.withDefault model.octaveStart params.octaveStart
                , octaveCount = Maybe.withDefault model.octaveCount params.octaveCount
            }

        Nothing ->
            applyQueryDefaults model


{-| Push the current state to the undo stack and clear the redo stack
-}
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

        -- Clear redo stack when new operation is performed
        newRedoStack =
            []
    in
    { model
        | undoStack = newUndoStack
        , redoStack = newRedoStack
    }


twinkleSong : SongConfig
twinkleSong =
    let
        kick =
            Instruments.percKick

        snare =
            Instruments.percSnare
    in
    { melody =
        -- "Twinkle twinkle little star"
        [ [ "C4", "C3", "E4" ], [ "C4" ], [ "G4", "G3", "B4" ], [ "G4" ] ]
            -- "how I wonder what"
            ++ [ [ "A4", "F3", "C5" ], [ "A4" ], [ "G4", "G3" ], [] ]
            -- "what you are so"
            ++ [ [ "F4", "F3", "A4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "E4" ] ]
            -- "far above the world"
            ++ [ [ "D4", "G3", "B4" ], [ "D4" ], [ "C4", "C3", "E4" ], [] ]
            -- "Up above the world"
            ++ [ [ "G4", "G3", "B4" ], [ "G4" ], [ "F4", "F3", "A4" ], [ "F4" ] ]
            -- "so high like a"
            ++ [ [ "E4", "C3", "G4" ], [ "E4" ], [ "D4", "G3", "B4" ], [ "D4" ] ]
            -- "diamond in the"
            ++ [ [ "G4", "C3", "E4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "D4" ] ]
            -- "sky (end)"
            ++ [ [ "C4", "C3", "E4" ], [], [], [] ]
    , percussion =
        -- "Twinkle twinkle little star"
        [ [ kick ], [], [ snare ], [] ]
            -- "how I wonder what"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "what you are so"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "far above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "Up above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "so high like a"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "diamond in the"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "sky (end)"
            ++ [ [ kick ], [], [ snare ], [] ]
    , bpm = 90
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


applySong : SongConfig -> Model -> Model
applySong sc model =
    { model
        | pitchGrid = Grid.convertMelodyToGrid sc.melody (scaleConfig model)
        , percGrid = Grid.convertPercussionToGrid sc.percussion
        , bpm = sc.bpm
        , octaveStart = sc.octaveStart
        , octaveCount = sc.octaveCount
        , bars = sc.bars
        , beatsPerBar = sc.beatsPerBar
        , subdivisions = sc.subdivisions
    }


type alias SongConfig =
    { melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , bpm : Int
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }



-- Sequencer Functions


getCurrentPlayingStep : Model -> Maybe Int
getCurrentPlayingStep model =
    case model.playState of
        PlayingStarted _ ->
            Just 0

        Playing { nextStep } ->
            Just (modBy (Timing.getTotalSteps (timeConfig model)) (nextStep - 1))

        Stopped ->
            Nothing


type alias NoteToPlay =
    { webAudioFont : String, midi : Int, duration : Float, volume : Float }


getActiveNotesForStep : Int -> Model -> List NoteToPlay
getActiveNotesForStep stepIdx model =
    let
        duration =
            Timing.noteDuration (timeConfig model)

        pitchNotes =
            times
                (\pitchIdx ->
                    let
                        position =
                            { pitchIdx = pitchIdx, stepIdx = stepIdx }
                    in
                    if Grid.isPitchCellActive position model.pitchGrid then
                        Just
                            { webAudioFont = Instruments.tonalWebAudioFont model.currentTonalInstrument
                            , midi = Scales.pitchIdxToMidi pitchIdx (scaleConfig model)
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                (Scales.getTotalPitches (scaleConfig model))
                |> List.filterMap identity

        drumConfig =
            Instruments.drumKitConfig model.currentDrumKit

        percNotes =
            Instruments.allPercTypes
                |> List.filterMap
                    (\percType ->
                        let
                            position =
                                { percType = percType, stepIdx = stepIdx }

                            ( webAudioFontName, midiNote ) =
                                case percType of
                                    _ ->
                                        if percType == Instruments.percKick then
                                            ( drumConfig.kickWebAudioFont, drumConfig.kickMidi )

                                        else
                                            ( drumConfig.snareWebAudioFont, drumConfig.snareMidi )
                        in
                        if Grid.isPercCellActive position model.percGrid then
                            Just
                                { webAudioFont = webAudioFontName
                                , midi = midiNote
                                , duration = duration
                                , volume = 0.8
                                }

                        else
                            Nothing
                    )
    in
    pitchNotes ++ percNotes
