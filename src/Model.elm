module Model exposing
    ( DrawState
    , Flags
    , Model
    , NoteToPlay
    , PlayState
    , ViewModel
    , changeBars
    , changeBeatsPerBar
    , changeOctaveCount
    , changeOctaveStart
    , changeRootNote
    , changeScaleType
    , changeSubdivisions
    , continueDrawingPerc
    , continueDrawingPitch
    , getSaveAction
    , init
    , loadFromUrl
    , onTimeSync
    , redo
    , setBPM
    , setDrumKit
    , setTonalInstrument
    , startDrawingPerc
    , startDrawingPitch
    , startPlaying
    , stop
    , stopDrawing
    , toVm
    , undo
    )

import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Songs exposing (SongConfig)
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
        |> (\m ->
                if url.query == Nothing then
                    applySong Songs.twinkleSong m

                else
                    m
           )


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
    , bpm : Int
    , currentTonalInstrument : TonalInstrument
    , currentDrumKit : DrumKit
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
    , pitchGrid : Maybe PitchGrid
    }


queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Pipeline.succeed QueryParams
        |> Pipeline.optional (Query.int "bpm")
        |> Pipeline.optional (Query.int "octaveStart")
        |> Pipeline.optional (Query.int "octaveCount")
        |> Pipeline.optional (Query.string "pitchGrid" |> Query.map (Maybe.andThen Grid.parsePitchGrid))


buildQuery : Model -> String
buildQuery model =
    UB.absolute
        []
        [ UB.int "bpm" model.bpm
        , UB.int "octaveStart" model.octaveStart
        , UB.int "octaveCount" model.octaveCount
        , UB.string "pitchGrid" (Grid.pitchGridToString model.pitchGrid)
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
    case parseQueryParams url |> Debug.log "parseQueryParams" of
        Just params ->
            { model
                | bpm = Maybe.withDefault model.bpm params.bpm
                , octaveStart = Maybe.withDefault model.octaveStart params.octaveStart
                , octaveCount = Maybe.withDefault model.octaveCount params.octaveCount
                , pitchGrid = Maybe.withDefault model.pitchGrid params.pitchGrid
            }

        Nothing ->
            applyQueryDefaults model


{-| Convert Model to HistoryState by extracting history-tracked fields
-}
toHistoryState : Model -> HistoryState
toHistoryState model =
    { pitchGrid = model.pitchGrid
    , percGrid = model.percGrid
    , scaleType = model.scaleType
    , rootNote = model.rootNote
    , octaveStart = model.octaveStart
    , octaveCount = model.octaveCount
    , bars = model.bars
    , beatsPerBar = model.beatsPerBar
    , subdivisions = model.subdivisions
    , bpm = model.bpm
    , currentTonalInstrument = model.currentTonalInstrument
    , currentDrumKit = model.currentDrumKit
    }


{-| Restore history-tracked fields from HistoryState into Model
-}
updateModelFromHistoryState : HistoryState -> Model -> Model
updateModelFromHistoryState historyState model =
    { model
        | pitchGrid = historyState.pitchGrid
        , percGrid = historyState.percGrid
        , scaleType = historyState.scaleType
        , rootNote = historyState.rootNote
        , octaveStart = historyState.octaveStart
        , octaveCount = historyState.octaveCount
        , bars = historyState.bars
        , beatsPerBar = historyState.beatsPerBar
        , subdivisions = historyState.subdivisions
        , bpm = historyState.bpm
        , currentTonalInstrument = historyState.currentTonalInstrument
        , currentDrumKit = historyState.currentDrumKit
    }


{-| Push the current state to the undo stack and clear the redo stack
-}
pushToHistory : Model -> Model
pushToHistory model =
    { model
        | undoStack = toHistoryState model :: model.undoStack
        , redoStack = []
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



-- Model Update Helper Functions


setBPM : Int -> Model -> Model
setBPM newBPM model =
    { model | bpm = atLeast 1 newBPM }


setTonalInstrument : TonalInstrument -> Model -> Model
setTonalInstrument newInstrument model =
    { model | currentTonalInstrument = newInstrument }


setDrumKit : DrumKit -> Model -> Model
setDrumKit newDrumKit model =
    { model | currentDrumKit = newDrumKit }


stopDrawing : Model -> Model
stopDrawing model =
    { model | drawState = NotDrawing }


changeScaleType : ScaleType -> Model -> Model
changeScaleType newScaleType model =
    let
        modelWithHistory =
            pushToHistory model

        newModel =
            { modelWithHistory | scaleType = newScaleType }

        newPitchGrid =
            Grid.resizePitchGrid (scaleConfig modelWithHistory) (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
    in
    { newModel | pitchGrid = newPitchGrid }


changeRootNote : RootNote -> Model -> Model
changeRootNote newRootNote model =
    let
        modelWithHistory =
            pushToHistory model

        newModel =
            { modelWithHistory | rootNote = newRootNote }

        newPitchGrid =
            Grid.transposePitchGrid (scaleConfig modelWithHistory) (scaleConfig newModel) modelWithHistory.pitchGrid
    in
    { newModel | pitchGrid = newPitchGrid }


changeOctaveStart : Int -> Model -> Model
changeOctaveStart newStart model =
    let
        modelWithHistory =
            pushToHistory model

        clampedStart =
            atLeast 1 newStart

        newModel =
            { modelWithHistory | octaveStart = clampedStart }

        newPitchGrid =
            Grid.resizePitchGrid (scaleConfig modelWithHistory) (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
    in
    { newModel | pitchGrid = newPitchGrid }


changeOctaveCount : Int -> Model -> Model
changeOctaveCount newCount model =
    let
        modelWithHistory =
            pushToHistory model

        clampedCount =
            atLeast 1 newCount

        newModel =
            { modelWithHistory | octaveCount = clampedCount }

        newPitchGrid =
            Grid.resizePitchGrid (scaleConfig modelWithHistory) (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
    in
    { newModel | pitchGrid = newPitchGrid }


startPlaying : Model -> Model
startPlaying model =
    case model.playState of
        Stopped ->
            setPlayState (PlayingStarted { startTime = model.audioContextTime }) model

        _ ->
            model


setPlayState playState model =
    { model | playState = playState }


stop : Model -> Model
stop model =
    case model.playState of
        PlayingStarted _ ->
            setPlayState Stopped model

        Playing _ ->
            setPlayState Stopped model

        Stopped ->
            model


loadFromUrl : Url -> Model -> Model
loadFromUrl url model =
    applyQueryParams url { model | url = url }


undo : Model -> Model
undo model =
    case model.undoStack of
        [] ->
            model

        lastState :: remainingUndoStack ->
            updateModelFromHistoryState lastState
                { model
                    | undoStack = remainingUndoStack
                    , redoStack = toHistoryState model :: model.redoStack
                }


redo : Model -> Model
redo model =
    case model.redoStack of
        [] ->
            model

        lastState :: remainingRedoStack ->
            let
                newUndoStack =
                    toHistoryState model :: model.undoStack

                newModel =
                    updateModelFromHistoryState lastState
                        { model
                            | undoStack = newUndoStack
                            , redoStack = remainingRedoStack
                        }
            in
            newModel


startDrawingPitch : PitchPos -> Model -> ( Model, Maybe NoteToPlay )
startDrawingPitch position model =
    case model.drawState of
        NotDrawing ->
            let
                modelWithHistory =
                    pushToHistory model

                currentlyActive =
                    Grid.isPitchCellActive position model.pitchGrid

                newDrawState =
                    if currentlyActive then
                        ErasingPitch

                    else
                        DrawingPitch

                newModel =
                    { modelWithHistory
                        | drawState = newDrawState
                        , pitchGrid = Grid.updatePitchCell position (not currentlyActive) modelWithHistory.pitchGrid
                    }

                maybeNote =
                    if not currentlyActive then
                        Just
                            { webAudioFont = Instruments.tonalWebAudioFont model.currentTonalInstrument
                            , midi = Scales.pitchIdxToMidi position.pitchIdx (scaleConfig model)
                            , duration = 0.5
                            , volume = 0.7
                            }

                    else
                        Nothing
            in
            ( newModel, maybeNote )

        _ ->
            ( model, Nothing )


continueDrawingPitch : PitchPos -> Model -> ( Model, Maybe NoteToPlay )
continueDrawingPitch position model =
    case model.drawState of
        DrawingPitch ->
            let
                newModel =
                    { model | pitchGrid = Grid.updatePitchCell position True model.pitchGrid }

                maybeNote =
                    Just
                        { webAudioFont = Instruments.tonalWebAudioFont model.currentTonalInstrument
                        , midi = Scales.pitchIdxToMidi position.pitchIdx (scaleConfig model)
                        , duration = 0.5
                        , volume = 0.7
                        }
            in
            ( newModel, maybeNote )

        ErasingPitch ->
            let
                newModel =
                    { model | pitchGrid = Grid.updatePitchCell position False model.pitchGrid }
            in
            ( newModel, Nothing )

        _ ->
            ( model, Nothing )


startDrawingPerc : PercPos -> Model -> ( Model, Maybe NoteToPlay )
startDrawingPerc position model =
    case model.drawState of
        NotDrawing ->
            let
                modelWithHistory =
                    pushToHistory model

                currentlyActive =
                    Grid.isPercCellActive position model.percGrid

                newDrawState =
                    if currentlyActive then
                        ErasingPerc

                    else
                        DrawingPerc

                newModel =
                    { modelWithHistory
                        | drawState = newDrawState
                        , percGrid = Grid.updatePercCell position (not currentlyActive) modelWithHistory.percGrid
                    }

                maybeNote =
                    if not currentlyActive then
                        let
                            drumConfig =
                                Instruments.drumKitConfig model.currentDrumKit

                            ( webAudioFontName, midiNote ) =
                                if position.percType == Instruments.percKick then
                                    ( drumConfig.kickWebAudioFont, drumConfig.kickMidi )

                                else
                                    ( drumConfig.snareWebAudioFont, drumConfig.snareMidi )
                        in
                        Just
                            { webAudioFont = webAudioFontName
                            , midi = midiNote
                            , duration = 0.5
                            , volume = 0.8
                            }

                    else
                        Nothing
            in
            ( newModel, maybeNote )

        _ ->
            ( model, Nothing )


continueDrawingPerc : PercPos -> Model -> ( Model, Maybe NoteToPlay )
continueDrawingPerc position model =
    case model.drawState of
        DrawingPerc ->
            let
                newModel =
                    { model | percGrid = Grid.updatePercCell position True model.percGrid }

                drumConfig =
                    Instruments.drumKitConfig model.currentDrumKit

                ( webAudioFontName, midiNote ) =
                    if position.percType == Instruments.percKick then
                        ( drumConfig.kickWebAudioFont, drumConfig.kickMidi )

                    else
                        ( drumConfig.snareWebAudioFont, drumConfig.snareMidi )

                maybeNote =
                    Just
                        { webAudioFont = webAudioFontName
                        , midi = midiNote
                        , duration = 0.5
                        , volume = 0.8
                        }
            in
            ( newModel, maybeNote )

        ErasingPerc ->
            let
                newModel =
                    { model | percGrid = Grid.updatePercCell position False model.percGrid }
            in
            ( newModel, Nothing )

        _ ->
            ( model, Nothing )


onTimeSync : Float -> Model -> ( Model, List NoteToPlay )
onTimeSync audioContextTime model =
    let
        updatedModel =
            { model | audioContextTime = audioContextTime }
    in
    case model.playState of
        PlayingStarted { startTime } ->
            let
                stepToSchedule =
                    0

                activeNotes =
                    getActiveNotesForStep stepToSchedule updatedModel

                newPlayState =
                    Playing { startTime = startTime, nextStep = 1 }
            in
            ( { updatedModel | playState = newPlayState }, activeNotes )

        Playing { startTime, nextStep } ->
            let
                elapsedTime =
                    audioContextTime - startTime

                duration =
                    Timing.noteDuration (timeConfig model)

                currentStep =
                    floor (elapsedTime / duration)
            in
            if currentStep >= nextStep then
                let
                    stepToSchedule =
                        modBy (Timing.getTotalSteps (timeConfig model)) nextStep

                    activeNotes =
                        getActiveNotesForStep stepToSchedule updatedModel

                    newPlayState =
                        Playing { startTime = startTime, nextStep = nextStep + 1 }
                in
                ( { updatedModel | playState = newPlayState }, activeNotes )

            else
                ( updatedModel, [] )

        Stopped ->
            ( updatedModel, [] )


changeBars : Int -> Model -> Model
changeBars newBars model =
    let
        modelWithHistory =
            pushToHistory model
    in
    { modelWithHistory | bars = atLeast 1 newBars }


changeBeatsPerBar : Int -> Model -> Model
changeBeatsPerBar newBeatsPerBar model =
    let
        modelWithHistory =
            pushToHistory model
    in
    { modelWithHistory | beatsPerBar = atLeast 1 newBeatsPerBar }


changeSubdivisions : Int -> Model -> Model
changeSubdivisions newSubdivisions model =
    let
        modelWithHistory =
            pushToHistory model
    in
    { modelWithHistory | subdivisions = atLeast 1 newSubdivisions }


getSaveAction : Model -> Maybe ( Nav.Key, String )
getSaveAction model =
    let
        query =
            buildQuery model
    in
    if model.url.query == Just query then
        Nothing

    else
        Just ( model.key, query )



-- ViewModel


type alias ViewModel =
    { totalSteps : Int
    , totalPitches : Int
    , canUndo : Bool
    , canRedo : Bool
    , isPlaying : Bool
    , isStepCurrentlyPlaying : Int -> Bool
    , isPitchCellActive : PitchPos -> Bool
    , isPercCellActive : PercPos -> Bool
    , isScaleSelected : ScaleType -> Bool
    , isRootNoteSelected : RootNote -> Bool
    , isTonalInstrumentSelected : TonalInstrument -> Bool
    , isDrumKitSelected : DrumKit -> Bool
    , pitchIdxToNoteName : Int -> String
    , bpm : Int
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }


toVm : Model -> ViewModel
toVm model =
    let
        totalSteps : Int
        totalSteps =
            Timing.getTotalSteps (timeConfig model)

        maybePlayingStepIdx =
            getCurrentPlayingStep model

        -- Extract computed values to avoid recalculation in helper functions
        scaleConfigValue =
            scaleConfig model
    in
    { totalSteps = totalSteps
    , totalPitches = Scales.getTotalPitches scaleConfigValue
    , canUndo = not (List.isEmpty model.undoStack)
    , canRedo = not (List.isEmpty model.redoStack)
    , isPlaying = model.playState /= Stopped
    , isStepCurrentlyPlaying = \stepIdx -> maybePlayingStepIdx == Just stepIdx
    , isPitchCellActive = \position -> Grid.isPitchCellActive position model.pitchGrid
    , isPercCellActive = \position -> Grid.isPercCellActive position model.percGrid
    , isScaleSelected = \scale -> model.scaleType == scale
    , isRootNoteSelected = \rootNote -> model.rootNote == rootNote
    , isTonalInstrumentSelected = \instrument -> model.currentTonalInstrument == instrument
    , isDrumKitSelected = \drumKit -> model.currentDrumKit == drumKit
    , pitchIdxToNoteName = \pitchIdx -> Scales.pitchIdxToNoteName pitchIdx scaleConfigValue
    , bpm = model.bpm
    , octaveStart = model.octaveStart
    , octaveCount = model.octaveCount
    , bars = model.bars
    , beatsPerBar = model.beatsPerBar
    , subdivisions = model.subdivisions
    }
