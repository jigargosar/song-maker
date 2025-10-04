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
    , deleteStep
    , init
    , loadFromUrl
    , loadSongByName
    , onTimeSync
    , playFromStep
    , redo
    , reset
    , setBPM
    , setDrumKit
    , setTonalInstrument
    , shiftStepRight
    , startDrawingPerc
    , startDrawingPitch
    , startPlaying
    , stop
    , stopDrawing
    , toQueryStringIfChanged
    , toVm
    , undo
    )

import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import QuerystringCodec as QC
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Songs exposing (SongConfig)
import Timing exposing (TimeConfig)
import Url exposing (Url)
import Utils exposing (..)


type alias Flags =
    {}


init : Flags -> Url -> Nav.Key -> Model
init _ url key =
    let
        initialModel : Model
        initialModel =
            { scaleType = Scales.Major
            , rootNote = Scales.C
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
    if url.query == Nothing then
        loadDefaultSong initialModel

    else
        QC.load url initialModel


loadDefaultSong : Model -> Model
loadDefaultSong model =
    loadSongByName "ode-to-joy" model


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


{-| CRITICAL: When adding ANY field to Model, decide: "Should this be tracked in history and URL?"

Ask: Is this user-facing state that should be:

  - Undoable (appears in undo/redo)?
  - Save-able (appears in URL query params)?

If YES → Update all 11 locations:

1.  Model type - ADD HERE FIRST
2.  HistoryState type
3.  QueryParams type - as Maybe Type
4.  toHistoryState function
5.  updateModelFromHistoryState function
6.  applyQueryDefaults function
7.  applyQueryParams function
8.  queryParser function
9.  buildQuery function
10. applySong function - if applicable
11. SongConfig type in Songs.elm - if songs should specify this field

If NO → Just add to Model type, nothing else needed

When adding/modifying a function that mutates history-tracked fields:

  - Add pushToHistory as first line if change should be undoable

-}
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


toQueryStringIfChanged : Model -> Maybe ( Nav.Key, String )
toQueryStringIfChanged model =
    QC.serialize model.url model
        |> Maybe.map (\queryString -> ( model.key, queryString ))


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


{-| Push the current state to the undo stack and clear the redo stack.
Returns unchanged model if state hasn't changed (prevents duplicate history entries).
-}
pushToHistory : Model -> Model
pushToHistory model =
    let
        currentState =
            toHistoryState model
    in
    if Just currentState == List.head model.undoStack then
        model

    else
        { model | undoStack = currentState :: model.undoStack, redoStack = [] }


applySong : SongConfig -> Model -> Model
applySong sc model =
    { model
        | pitchGrid = Grid.convertMelodyToGrid sc.melody
        , percGrid = Grid.convertPercussionToGrid sc.percussion
        , scaleType = Scales.Major
        , rootNote = Scales.C
        , bpm = sc.bpm
        , octaveStart = sc.octaveStart
        , octaveCount = sc.octaveCount
        , bars = sc.bars
        , beatsPerBar = sc.beatsPerBar
        , subdivisions = sc.subdivisions
    }


loadSongByName : String -> Model -> Model
loadSongByName songName model =
    case Songs.parseSong songName of
        Just songConfig ->
            model
                |> pushToHistory
                |> applySong songConfig

        Nothing ->
            model


reset : Model -> Maybe ( Model, Nav.Key )
reset model =
    let
        resetModel =
            model
                |> pushToHistory
                |> QC.reset
    in
    if resetModel == model then
        Nothing

    else
        Just ( resetModel, model.key )


shiftStepRight : Int -> Model -> Model
shiftStepRight fromStepIdx model =
    let
        modelWithHistory =
            pushToHistory model

        totalSteps =
            Timing.getTotalSteps (timeConfig model)

        ( newPitchGrid, newPercGrid ) =
            Grid.shiftStepRight fromStepIdx totalSteps modelWithHistory.pitchGrid modelWithHistory.percGrid
    in
    { modelWithHistory | pitchGrid = newPitchGrid, percGrid = newPercGrid }


deleteStep : Int -> Model -> Model
deleteStep stepToDelete model =
    let
        modelWithHistory =
            pushToHistory model

        totalSteps =
            Timing.getTotalSteps (timeConfig model)

        ( newPitchGrid, newPercGrid ) =
            Grid.deleteStep stepToDelete totalSteps modelWithHistory.pitchGrid modelWithHistory.percGrid
    in
    { modelWithHistory | pitchGrid = newPitchGrid, percGrid = newPercGrid }



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
                    if Grid.isPitchCellActive position (scaleConfig model) model.pitchGrid then
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
            Grid.resizePitchGrid (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
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
            Grid.resizePitchGrid (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
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
            Grid.resizePitchGrid (scaleConfig newModel) (timeConfig newModel) modelWithHistory.pitchGrid
    in
    { newModel | pitchGrid = newPitchGrid }


startPlaying : Model -> Model
startPlaying model =
    case model.playState of
        Stopped ->
            setPlayState (PlayingStarted { startTime = model.audioContextTime }) model

        _ ->
            model


playFromStep : Int -> Model -> Model
playFromStep stepIdx model =
    let
        adjustedStartTime =
            model.audioContextTime - (toFloat stepIdx * Timing.noteDuration (timeConfig model))
    in
    setPlayState (Playing { startTime = adjustedStartTime, nextStep = stepIdx + 1 }) model


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
    let
        newModel =
            { model | url = url }
    in
    if url.query == Nothing then
        QC.reset newModel

    else
        QC.load url newModel


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
                    Grid.isPitchCellActive position (scaleConfig model) model.pitchGrid

                newDrawState =
                    if currentlyActive then
                        ErasingPitch

                    else
                        DrawingPitch

                newModel =
                    { modelWithHistory
                        | drawState = newDrawState
                        , pitchGrid = Grid.updatePitchCell position (scaleConfig modelWithHistory) (not currentlyActive) modelWithHistory.pitchGrid
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
                    { model | pitchGrid = Grid.updatePitchCell position (scaleConfig model) True model.pitchGrid }

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
                    { model | pitchGrid = Grid.updatePitchCell position (scaleConfig model) False model.pitchGrid }
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
    , isPitchCellActive = \position -> Grid.isPitchCellActive position scaleConfigValue model.pitchGrid
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



{- TODO:
   - Reset undo/redo stacks in loadFromUrl to prevent stale history
-}
{- GRID ARCHITECTURE (Post-Refactoring)

   DATA STRUCTURE:
   - Grid stores Set (MidiNote, StepIdx) where MidiNote is absolute MIDI number (60 = C4)
   - ScaleConfig (scaleType/rootNote/octaveStart/octaveCount) determines which MIDI notes are visible
   - TimeConfig (bars/beatsPerBar/subdivisions) determines total steps

   KEY OPERATIONS:
   1. Load song: "C4" → MIDI 60 via noteNameToMidi, insert directly (O(1))
   2. Click cell: Convert pitchIdx → MIDI via pitchIdxToMidi, insert directly
   3. Transpose (root change): Map over grid adding semitone delta: (midi, step) → (midi + Δ, step)
   4. Scale change: Filter grid, keep only MIDI notes valid in new scale (invalid notes dropped)
   5. Render: Check if MIDI note exists in grid, convert pitchIdx → MIDI for lookup
   6. Play: Use MIDI values directly from grid

   GRID DIMENSIONS:
   - Height: Computed from scaleType + octaveStart + octaveCount
     Example: C Major, octave 4, count 2 → [60,62,64,65,67,69,71,72,74,76,77,79,81,83] = 14 rows
   - Width: Computed from bars × beatsPerBar × subdivisions
     Example: 4 bars × 4 beats × 2 subdivisions = 32 columns

   SERIALIZATION FORMAT (Breaking Change):
   - Old: pitchGrid=0,0,1,2 (scale-relative indices)
   - New: pitchGrid=60,0,62,2 (absolute MIDI notes)
   - URLs from before this refactoring will produce incorrect notes

   BENEFITS ACHIEVED:
   - Simpler mental model (MIDI is universal, not scale-dependent)
   - Faster song loading (direct noteNameToMidi conversion, no loops)
   - Simpler transposition (just semitone addition via Set.map)
   - Removed dead code (noteNameToPitchIdx, pitchIdxToScaleDegree, scaleDegreeToPitchIdx)

-}
