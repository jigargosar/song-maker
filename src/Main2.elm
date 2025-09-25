port module Main2 exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Set exposing (Set)


{-| <https://surikov.github.io/webaudiofont/>
-}



-- PORTS


port playNote : { instrument : String, midi : Int, duration : Float, volume : Float } -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- AUDIO CONSTANTS


midiC4 : Int
midiC4 =
    60



-- SCALE SYSTEM


type ScaleType
    = Major
    | Pentatonic
    | Chromatic


type RootNote
    = C
    | CSharp
    | D
    | DSharp
    | E
    | F
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B


getScalePattern : ScaleType -> List Int
getScalePattern scaleType =
    case scaleType of
        Major ->
            [ 0, 2, 4, 5, 7, 9, 11 ]

        Pentatonic ->
            [ 0, 2, 4, 7, 9 ]

        Chromatic ->
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]


getRootNoteOffset : RootNote -> Int
getRootNoteOffset rootNote =
    case rootNote of
        C ->
            0

        CSharp ->
            1

        D ->
            2

        DSharp ->
            3

        E ->
            4

        F ->
            5

        FSharp ->
            6

        G ->
            7

        GSharp ->
            8

        A ->
            9

        ASharp ->
            10

        B ->
            11


chromaticNoteNames : List String
chromaticNoteNames =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


notesPerOctave : ScaleType -> Int
notesPerOctave scaleType =
    List.length (getScalePattern scaleType)


getTotalPitches : Model -> Int
getTotalPitches model =
    notesPerOctave model.scaleType * model.octaveRange.count


pitchIdxToMidi : Int -> Model -> Int
pitchIdxToMidi pitchIdx model =
    let
        scalePattern =
            getScalePattern model.scaleType

        rootOffset =
            getRootNoteOffset model.rootNote

        notesInScale =
            notesPerOctave model.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            model.octaveRange.start + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        baseC0 =
            12
    in
    if octaveIdx < model.octaveRange.count then
        baseC0 + (octave * 12) + rootOffset + semitone

    else
        midiC4


pitchIdxToNoteName : Int -> Model -> String
pitchIdxToNoteName pitchIdx model =
    let
        scalePattern =
            getScalePattern model.scaleType

        rootOffset =
            getRootNoteOffset model.rootNote

        notesInScale =
            notesPerOctave model.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            model.octaveRange.start + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        chromaticIndex =
            modBy 12 (rootOffset + semitone)

        noteName =
            Maybe.withDefault "?" (List.drop chromaticIndex chromaticNoteNames |> List.head)
    in
    if octaveIdx < model.octaveRange.count then
        noteName ++ String.fromInt octave

    else
        "C4"



-- Standard snare perc MIDI note


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )


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


type alias SongConfig =
    { melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , totalSteps : Int
    , bpm : Int
    , octaveRange : { start : Int, count : Int }
    }


type alias Model =
    { scaleType : ScaleType
    , rootNote : RootNote
    , octaveRange : { start : Int, count : Int }
    , totalSteps : Int
    , pitchGrid : PitchGrid
    , percGrid : PercGrid
    , drawState : DrawState
    , playState : PlayState
    , audioContextTime : Float
    , bpm : Int
    , currentTonalInstrument : TonalInstrument
    , currentDrumKit : DrumKit
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { scaleType = Major
      , rootNote = C
      , octaveRange = { start = 3, count = 3 }
      , totalSteps = 32
      , pitchGrid = Set.empty
      , percGrid = Set.empty
      , drawState = NotDrawing
      , playState = Stopped
      , audioContextTime = 0.0
      , bpm = 120
      , currentTonalInstrument = Instruments.GrandPianoSBLive
      , currentDrumKit = Instruments.StandardKit
      }
        |> applySong twinkleSong
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartDrawingPitch PitchPos
    | ContinueDrawingPitch PitchPos
    | StopDrawing
    | StartDrawingPerc PercPos
    | ContinueDrawingPerc PercPos
    | PlayPitchNote Int
    | PlayPercNote PercType
    | Play
    | Stop
    | TimeSync Float
    | ChangeScaleType ScaleType
    | ChangeRootNote RootNote
    | ChangeOctaveStart Int
    | ChangeOctaveCount Int
    | ChangeBPM Int
    | ChangeTonalInstrument TonalInstrument
    | ChangeDrumKit DrumKit


subscriptions : Model -> Sub Msg
subscriptions _ =
    timeSync TimeSync


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawingPitch position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isPitchCellActive position model.pitchGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPitch

                            else
                                DrawingPitch
                    in
                    ( { model
                        | drawState = newDrawState
                        , pitchGrid = updatePitchCell position (not currentlyActive) model.pitchGrid
                      }
                    , playPitchCmdIf (not currentlyActive) position.pitchIdx model
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPitch position ->
            case model.drawState of
                DrawingPitch ->
                    ( { model | pitchGrid = updatePitchCell position True model.pitchGrid }
                    , playPitchCmdIf True position.pitchIdx model
                    )

                ErasingPitch ->
                    ( { model | pitchGrid = updatePitchCell position False model.pitchGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        StartDrawingPerc position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isPercCellActive position model.percGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPerc

                            else
                                DrawingPerc
                    in
                    ( { model
                        | drawState = newDrawState
                        , percGrid = updatePercCell position (not currentlyActive) model.percGrid
                      }
                    , playPercCmdIf (not currentlyActive) position.percType model
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPerc position ->
            case model.drawState of
                DrawingPerc ->
                    ( { model | percGrid = updatePercCell position True model.percGrid }
                    , playPercCmdIf True position.percType model
                    )

                ErasingPerc ->
                    ( { model | percGrid = updatePercCell position False model.percGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayPitchNote pitchIdx ->
            ( model, playPitchCmdIf True pitchIdx model )

        PlayPercNote percType ->
            ( model, playPercCmdIf True percType model )

        Play ->
            case model.playState of
                Stopped ->
                    ( { model | playState = PlayingStarted { startTime = model.audioContextTime } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Stop ->
            case model.playState of
                PlayingStarted _ ->
                    ( { model | playState = Stopped }, Cmd.none )

                Playing _ ->
                    ( { model | playState = Stopped }, Cmd.none )

                Stopped ->
                    ( model, Cmd.none )

        TimeSync audioContextTime ->
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

                        playCommands =
                            List.map playNote activeNotes |> Cmd.batch

                        newPlayState =
                            Playing { startTime = startTime, nextStep = 1 }
                    in
                    ( { updatedModel | playState = newPlayState }, playCommands )

                Playing { startTime, nextStep } ->
                    let
                        elapsedTime =
                            audioContextTime - startTime

                        duration =
                            noteDuration model

                        currentStep =
                            floor (elapsedTime / duration)
                    in
                    if currentStep >= nextStep then
                        let
                            stepToSchedule =
                                modBy model.totalSteps nextStep

                            activeNotes =
                                getActiveNotesForStep stepToSchedule updatedModel

                            playCommands =
                                List.map playNote activeNotes |> Cmd.batch

                            newPlayState =
                                Playing { startTime = startTime, nextStep = nextStep + 1 }
                        in
                        ( { updatedModel | playState = newPlayState }, playCommands )

                    else
                        ( updatedModel, Cmd.none )

                Stopped ->
                    ( updatedModel, Cmd.none )

        ChangeScaleType newScaleType ->
            let
                newModel =
                    { model | scaleType = newScaleType }

                newPitchGrid =
                    resizePitchGrid model newModel model.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeRootNote newRootNote ->
            let
                newModel =
                    { model | rootNote = newRootNote }

                newPitchGrid =
                    resizePitchGrid model newModel model.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeOctaveStart newStart ->
            let
                clampedStart =
                    max 1 newStart

                newModel =
                    { model | octaveRange = { start = clampedStart, count = model.octaveRange.count } }

                newPitchGrid =
                    resizePitchGrid model newModel model.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeOctaveCount newCount ->
            let
                clampedCount =
                    max 1 newCount

                newModel =
                    { model | octaveRange = { start = model.octaveRange.start, count = clampedCount } }

                newPitchGrid =
                    resizePitchGrid model newModel model.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeBPM newBPM ->
            ( { model | bpm = max 1 newBPM }
            , Cmd.none
            )

        ChangeTonalInstrument newInstrument ->
            ( { model | currentTonalInstrument = newInstrument }
            , Cmd.none
            )

        ChangeDrumKit newDrumKit ->
            ( { model | currentDrumKit = newDrumKit }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-gray-900 text-white flex flex-col select-none" ]
        [ viewHeader model
        , centerView model
        , footerView
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bg-gray-800 z-20 shadow-2xl border-b border-gray-950 px-6 py-4" ]
        [ div [ class "flex flex-wrap items-center justify-between" ]
            [ div [ class "flex flex-wrap items-center gap-6" ]
                [ div [ class "text-2xl font-bold text-white" ] [ text "Song Maker V2" ]
                , viewScaleControls model
                , viewInstrumentControls model
                , viewControlGroup "BPM" (viewBPMInput model.bpm)
                , viewPlayStopButton model.playState
                ]
            ]
        ]


centerView : Model -> Html Msg
centerView model =
    div [ class "flex-1 overflow-auto" ] [ viewGrid model ]


viewGrid : Model -> Html Msg
viewGrid model =
    let
        currentStep =
            getCurrentPlayingStep model

        gridTemplateCols =
            format "minmax($pitchLabelColMinWidth, auto) repeat($totalSteps, minmax($stepColMinWidth, 1fr))"
                [ ( "$pitchLabelColMinWidth", px 48 )
                , ( "$totalSteps", String.fromInt model.totalSteps )
                , ( "$stepColMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalPitches, minmax($pitchRowMinHeight, 1fr)) repeat(2, $percRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalPitches", String.fromInt (getTotalPitches model) )
                , ( "$pitchRowMinHeight", px 32 )
                , ( "$percRowHeight", px 48 )
                ]
    in
    div
        [ class "grid bg-gray-800 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateCols
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class labelBgColorAndClass, class "border-b border-gray-600" ] [] ]
            ++ {- Step Labels row -} times (\stepIdx -> viewStepLabel currentStep stepIdx) model.totalSteps
            ++ {- Pitch rows -} (times (\pitchIdx -> viewPitchRow model model.pitchGrid currentStep pitchIdx) (getTotalPitches model) |> List.concat)
            ++ {- Perc Snare row -} viewPercRow Instruments.Snare model.totalSteps model.percGrid currentStep
            ++ {- Perc Kick row -} viewPercRow Instruments.Kick model.totalSteps model.percGrid currentStep
        )


viewStepLabel : Maybe Int -> Int -> Html Msg
viewStepLabel currentStep stepIdx =
    let
        isCurrentStep =
            currentStep == Just stepIdx

        bgClass =
            if isCurrentStep then
                accentBgColor

            else
                labelBgColor
    in
    div
        [ class labelClass, class bgClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepIdx + 1)) ]


viewPitchRow : Model -> PitchGrid -> Maybe Int -> Int -> List (Html Msg)
viewPitchRow model pitchGrid currentStep pitchIdx =
    let
        viewPitchLabel =
            div
                [ class labelBgColorAndClass, class "border-[0.5px]" ]
                [ text (pitchIdxToNoteName pitchIdx model) ]
    in
    -- TODO: Should we fix function parameters?
    viewPitchLabel :: times (\stepIdx -> viewPitchCell pitchIdx pitchGrid currentStep stepIdx) model.totalSteps


viewPitchCell : Int -> PitchGrid -> Maybe Int -> Int -> Html Msg
viewPitchCell pitchIdx pitchGrid currentStep stepIdx =
    let
        position =
            { pitchIdx = pitchIdx, stepIdx = stepIdx }

        isActive =
            isPitchCellActive position pitchGrid

        isCurrentStep =
            currentStep == Just stepIdx

        -- TODO: try refactor
        noteClass =
            if isActive then
                pitchCellColor pitchIdx

            else if isCurrentStep then
                "bg-gray-700 hover:bg-gray-600"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class noteClass
        , class "border-[0.5px] border-gray-600 cursor-pointer "
        , HE.onMouseDown (StartDrawingPitch position)
        , HE.onMouseEnter (ContinueDrawingPitch position)
        , HE.onMouseUp StopDrawing
        ]
        []


viewPercRow : PercType -> Int -> PercGrid -> Maybe Int -> List (Html Msg)
viewPercRow percType totalSteps percGrid currentStep =
    let
        percTypeName =
            Instruments.percLabel percType

        stickyClass =
            case percType of
                Instruments.Snare ->
                    "sticky bottom-12 h-12 z-10 border-t-3"

                Instruments.Kick ->
                    "sticky bottom-0 h-12 z-10"
    in
    div [ class labelBgColorAndClass, class stickyClass ] [ text percTypeName ]
        :: times (\stepIdx -> viewPercCell percType percGrid currentStep stepIdx) totalSteps


viewPercCell : PercType -> PercGrid -> Maybe Int -> Int -> Html Msg
viewPercCell percType percGrid currentStep stepIdx =
    let
        position =
            { percType = percType, stepIdx = stepIdx }

        isActive =
            isPercCellActive position percGrid

        isCurrentStep =
            currentStep == Just stepIdx

        symbol =
            viewPercSymbol isActive percType

        stickyClass =
            case percType of
                Instruments.Snare ->
                    "sticky bottom-12 h-12 z-10  border-t-3"

                Instruments.Kick ->
                    "sticky bottom-0 h-12 z-10"

        cellClass =
            if isCurrentStep then
                -- TODO: do we need rings here?
                "bg-gray-700 hover:bg-gray-600"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class " border-gray-600 cursor-pointer  flex items-center justify-center"
        , class cellClass
        , class stickyClass
        , HE.onMouseDown (StartDrawingPerc position)
        , HE.onMouseEnter (ContinueDrawingPerc position)
        , HE.onMouseUp StopDrawing
        ]
        [ symbol ]


footerView : Html Msg
footerView =
    div [ class "bg-gray-800 border-t border-gray-700 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-gray-400" ]
            [ div [] [ text "Ready to rock 🎸" ]
            , div [] [ text "Time: 0.0s" ]
            , div [] [ text "V2 - Clean Architecture" ]
            ]
        ]



-- Conversion Functions


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Sequencer Functions


noteDuration : Model -> Float
noteDuration model =
    60.0 / toFloat model.bpm


getCurrentPlayingStep : Model -> Maybe Int
getCurrentPlayingStep model =
    case model.playState of
        PlayingStarted _ ->
            Just 0

        Playing { nextStep } ->
            Just (modBy model.totalSteps (nextStep - 1))

        Stopped ->
            Nothing


type alias NoteToPlay =
    { instrument : String, midi : Int, duration : Float, volume : Float }


getActiveNotesForStep : Int -> Model -> List NoteToPlay
getActiveNotesForStep stepIdx model =
    let
        duration =
            noteDuration model

        pitchNotes =
            times
                (\pitchIdx ->
                    let
                        position =
                            { pitchIdx = pitchIdx, stepIdx = stepIdx }
                    in
                    if isPitchCellActive position model.pitchGrid then
                        Just
                            { instrument = Instruments.tonalJsVarName model.currentTonalInstrument
                            , midi = pitchIdxToMidi pitchIdx model
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                (getTotalPitches model)
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

                            ( instrumentName, midiNote ) =
                                case percType of
                                    Instruments.Kick ->
                                        ( drumConfig.kickInstrument, drumConfig.kickMidi )

                                    Instruments.Snare ->
                                        ( drumConfig.snareInstrument, drumConfig.snareMidi )
                        in
                        if isPercCellActive position model.percGrid then
                            Just
                                { instrument = instrumentName
                                , midi = midiNote
                                , duration = duration
                                , volume = 0.8
                                }

                        else
                            Nothing
                    )
    in
    pitchNotes ++ percNotes



-- Audio Helper Functions


playPitchCmdIf : Bool -> Int -> Model -> Cmd Msg
playPitchCmdIf shouldPlay pitchIdx model =
    if shouldPlay then
        playNote
            { instrument = Instruments.tonalJsVarName model.currentTonalInstrument
            , midi = pitchIdxToMidi pitchIdx model
            , duration = 0.5
            , volume = 0.7
            }

    else
        Cmd.none


playPercCmdIf : Bool -> PercType -> Model -> Cmd Msg
playPercCmdIf shouldPlay percType model =
    if shouldPlay then
        let
            drumConfig =
                Instruments.drumKitConfig model.currentDrumKit

            ( instrumentName, midiNote ) =
                case percType of
                    Instruments.Kick ->
                        ( drumConfig.kickInstrument, drumConfig.kickMidi )

                    Instruments.Snare ->
                        ( drumConfig.snareInstrument, drumConfig.snareMidi )
        in
        playNote
            { instrument = instrumentName
            , midi = midiNote
            , duration = 0.5
            , volume = 0.8
            }

    else
        Cmd.none



-- Grid Resize Functions


resizePitchGrid : Model -> Model -> PitchGrid -> PitchGrid
resizePitchGrid oldModel newModel existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    midiPitch =
                        pitchIdxToMidi pitchIdx oldModel

                    newPitchIdx =
                        midiToPitchIdx midiPitch newModel
                in
                if newPitchIdx >= 0 && newPitchIdx < getTotalPitches newModel && stepIdx < newModel.totalSteps then
                    Just ( newPitchIdx, stepIdx )

                else
                    Nothing
            )
        |> Set.fromList


midiToPitchIdx : Int -> Model -> Int
midiToPitchIdx targetMidi model =
    let
        totalPitches =
            getTotalPitches model
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToMidi pitchIdx model == targetMidi)
        |> List.head
        |> Maybe.withDefault -1


noteNameToPitchIdx : String -> Model -> Int
noteNameToPitchIdx noteName model =
    let
        totalPitches =
            getTotalPitches model
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToNoteName pitchIdx model == noteName)
        |> List.head
        |> Maybe.withDefault -1


twinkleSong : SongConfig
twinkleSong =
    let
        kick =
            Instruments.Kick

        snare =
            Instruments.Snare
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
    , totalSteps = 32
    , bpm = 180
    , octaveRange = { start = 3, count = 3 }
    }


applySong : SongConfig -> Model -> Model
applySong songConfig model =
    let
        pitchGrid =
            convertMelodyToGrid songConfig.melody model

        percGrid =
            convertPercussionToGrid songConfig.percussion
    in
    { model
        | pitchGrid = pitchGrid
        , percGrid = percGrid
        , totalSteps = songConfig.totalSteps
        , bpm = songConfig.bpm
        , octaveRange = songConfig.octaveRange
    }


convertMelodyToGrid : List (List String) -> Model -> PitchGrid
convertMelodyToGrid stepMelodies model =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        let
                            pitchIdx =
                                noteNameToPitchIdx noteName model
                        in
                        if pitchIdx >= 0 then
                            Just ( pitchIdx, stepIdx )

                        else
                            Nothing
                    )
                    noteNames
            )
        |> List.concat
        |> Set.fromList


convertPercussionToGrid : List (List PercType) -> PercGrid
convertPercussionToGrid stepPercussion =
    stepPercussion
        |> List.indexedMap
            (\stepIdx percTypes ->
                List.map
                    (\percType ->
                        ( Instruments.percRowIdx percType, stepIdx )
                    )
                    percTypes
            )
        |> List.concat
        |> Set.fromList



-- Cell State Management


isPitchCellActive : PitchPos -> PitchGrid -> Bool
isPitchCellActive { pitchIdx, stepIdx } pitchGrid =
    Set.member ( pitchIdx, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchIdx, stepIdx ) pitchGrid

    else
        Set.remove ( pitchIdx, stepIdx ) pitchGrid


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    Set.member (percPositionToTuple position) grid


updatePercCell : PercPos -> Bool -> PercGrid -> PercGrid
updatePercCell position isActive grid =
    let
        tuple =
            percPositionToTuple position
    in
    if isActive then
        Set.insert tuple grid

    else
        Set.remove tuple grid



-- Style Constants


accentBgColor : String
accentBgColor =
    "bg-[oklch(55%_0.18_180)]"


accentBgColorHover : String
accentBgColorHover =
    "hover:bg-[oklch(59%_0.20_180)]"


accentBgColorWithHover : String
accentBgColorWithHover =
    accentBgColor ++ " " ++ accentBgColorHover


labelBgColorAndClass =
    labelClass ++ " " ++ labelBgColor


labelClass : String
labelClass =
    "border-r border-gray-600 flex items-center justify-center text-xs font-bold text-white"


labelBgColor =
    "bg-gray-900"



-- View Helpers


viewPlayStopButton : PlayState -> Html Msg
viewPlayStopButton playState =
    let
        ( buttonText, buttonMsg ) =
            case playState of
                Stopped ->
                    ( "Play", Play )

                _ ->
                    ( "Stop", Stop )
    in
    div
        [ class accentBgColorWithHover
        , class "text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer"
        , HE.onClick buttonMsg
        ]
        [ text buttonText ]


viewPercSymbol : Bool -> PercType -> Html Msg
viewPercSymbol isActive percType =
    if isActive then
        case percType of
            Instruments.Kick ->
                -- Circle symbol
                div [ class "w-6 h-6 rounded-full", class accentBgColor ] []

            Instruments.Snare ->
                -- Triangle symbol
                div [ class "w-6 h-6", class accentBgColor, style "clip-path" "polygon(50% 0%, 0% 100%, 100% 100%)" ] []

    else
        -- Small dim dot for inactive
        div [ class "w-1.5 h-1.5 bg-gray-500 rounded-full" ] []


{-| TODO: why mod 7 and how it works
it works for major and pentatonic, since we are doing mod by7
but doesnt work for chromatic
-}
pitchCellColor : Int -> String
pitchCellColor pitchIdx =
    case modBy 7 pitchIdx of
        0 ->
            "bg-[oklch(70%_0.13_0)] hover:bg-[oklch(74%_0.16_0)] transition-colors"

        -- C - Red (both C4 and C5)
        1 ->
            "bg-[oklch(70%_0.13_35)] hover:bg-[oklch(74%_0.16_35)] transition-colors"

        -- D - Orange
        2 ->
            "bg-[oklch(70%_0.13_70)] hover:bg-[oklch(74%_0.16_70)] transition-colors"

        -- E - Yellow
        3 ->
            "bg-[oklch(70%_0.13_120)] hover:bg-[oklch(74%_0.16_120)] transition-colors"

        -- F - Green
        4 ->
            "bg-[oklch(70%_0.13_210)] hover:bg-[oklch(74%_0.16_210)] transition-colors"

        -- G - Blue
        5 ->
            "bg-[oklch(70%_0.13_270)] hover:bg-[oklch(74%_0.16_270)] transition-colors"

        -- A - Purple
        6 ->
            "bg-[oklch(70%_0.13_310)] hover:bg-[oklch(74%_0.16_310)] transition-colors"

        -- B - Magenta
        _ ->
            "bg-[oklch(60%_0.02_0)] hover:bg-[oklch(64%_0.05_0)] transition-colors"


viewScaleControls : Model -> Html Msg
viewScaleControls model =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Scale" (viewScaleTypeSelector model.scaleType)
        , viewControlGroup "Root" (viewRootNoteSelector model.rootNote)
        , viewControlGroup "Start" (viewOctaveStartInput model.octaveRange.start)
        , viewControlGroup "Count" (viewOctaveCountInput model.octaveRange.count)
        ]


viewInstrumentControls : Model -> Html Msg
viewInstrumentControls model =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Instrument" (viewTonalInstrumentSelector model.currentTonalInstrument)
        , viewControlGroup "Drums" (viewDrumKitSelector model.currentDrumKit)
        ]


viewControlGroup : String -> Html Msg -> Html Msg
viewControlGroup labelText control =
    div [ class "flex flex-col items-center gap-1" ]
        [ H.label [ class "text-xs text-gray-400 font-medium" ]
            [ text labelText ]
        , control
        ]


viewScaleTypeSelector : ScaleType -> Html Msg
viewScaleTypeSelector currentScale =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (parseScaleType >> ChangeScaleType)
        ]
        [ H.option [ HA.value "Major", HA.selected (currentScale == Major) ] [ text "Major" ]
        , H.option [ HA.value "Pentatonic", HA.selected (currentScale == Pentatonic) ] [ text "Pentatonic" ]
        , H.option [ HA.value "Chromatic", HA.selected (currentScale == Chromatic) ] [ text "Chromatic" ]
        ]


viewRootNoteSelector : RootNote -> Html Msg
viewRootNoteSelector currentRoot =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (parseRootNote >> ChangeRootNote)
        ]
        (List.map (viewRootNoteOption currentRoot) allRootNotes)


viewRootNoteOption : RootNote -> RootNote -> Html Msg
viewRootNoteOption currentRoot rootNote =
    H.option
        [ HA.value (rootNoteToString rootNote)
        , HA.selected (currentRoot == rootNote)
        ]
        [ text (rootNoteToString rootNote) ]


viewOctaveStartInput : Int -> Html Msg
viewOctaveStartInput currentStart =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentStart)
        , HE.onInput (String.toInt >> Maybe.withDefault 4 >> ChangeOctaveStart)
        ]
        []


viewOctaveCountInput : Int -> Html Msg
viewOctaveCountInput currentCount =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentCount)
        , HE.onInput (String.toInt >> Maybe.withDefault 2 >> ChangeOctaveCount)
        ]
        []


viewBPMInput : Int -> Html Msg
viewBPMInput currentBPM =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentBPM)
        , HE.onInput (String.toInt >> Maybe.withDefault 120 >> ChangeBPM)
        ]
        []


allRootNotes : List RootNote
allRootNotes =
    [ C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B ]


rootNoteToString : RootNote -> String
rootNoteToString rootNote =
    case rootNote of
        C ->
            "C"

        CSharp ->
            "C#"

        D ->
            "D"

        DSharp ->
            "D#"

        E ->
            "E"

        F ->
            "F"

        FSharp ->
            "F#"

        G ->
            "G"

        GSharp ->
            "G#"

        A ->
            "A"

        ASharp ->
            "A#"

        B ->
            "B"


parseScaleType : String -> ScaleType
parseScaleType str =
    case str of
        "Major" ->
            Major

        "Pentatonic" ->
            Pentatonic

        "Chromatic" ->
            Chromatic

        _ ->
            Major


parseRootNote : String -> RootNote
parseRootNote str =
    case str of
        "C" ->
            C

        "C#" ->
            CSharp

        "D" ->
            D

        "D#" ->
            DSharp

        "E" ->
            E

        "F" ->
            F

        "F#" ->
            FSharp

        "G" ->
            G

        "G#" ->
            GSharp

        "A" ->
            A

        "A#" ->
            ASharp

        "B" ->
            B

        _ ->
            C


viewTonalInstrumentSelector : TonalInstrument -> Html Msg
viewTonalInstrumentSelector currentInstrument =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseTonal >> ChangeTonalInstrument)
        ]
        (List.map (viewTonalInstrumentOption currentInstrument) Instruments.allTonal)


viewDrumKitSelector : DrumKit -> Html Msg
viewDrumKitSelector currentDrumKit =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseDrumKit >> ChangeDrumKit)
        ]
        (List.map (viewDrumKitOption currentDrumKit) Instruments.allDrumKits)


viewTonalInstrumentOption : TonalInstrument -> TonalInstrument -> Html Msg
viewTonalInstrumentOption currentInstrument instrument =
    H.option
        [ HA.value (Instruments.tonalLabel instrument)
        , HA.selected (currentInstrument == instrument)
        ]
        [ text (Instruments.tonalLabel instrument) ]


viewDrumKitOption : DrumKit -> DrumKit -> Html Msg
viewDrumKitOption currentDrumKit drumKit =
    H.option
        [ HA.value (Instruments.drumKitLabel drumKit)
        , HA.selected (currentDrumKit == drumKit)
        ]
        [ text (Instruments.drumKitLabel drumKit) ]



-- BASIC VIEW UTILS


px : Float -> String
px f =
    String.fromFloat f ++ "px"



-- BASIC UTILS


format : String -> List ( String, String ) -> String
format templateString replacements =
    List.foldr (\( a, b ) -> String.replace a b) templateString replacements


times fn i =
    List.range 0 (i - 1) |> List.map fn



{- REFACTORING PLAN: Extract MusicTheory Module


   ## Goal

   Extract pure music theory code into a separate MusicTheory.elm module while maintaining clean boundaries and dependencies.


   ## Module Design


   ### MusicTheory.elm (Pure Functions - No Dependencies)

       module MusicTheory exposing
           ( RootNote(..)
           , ScaleType(..)
           , allRootNotes
           , allScaleTypes
           , chromaticNoteNames
           , getRootNoteOffset
           , getScalePattern
           , notesPerOctave
           , parseRootNote
           , parseScaleType
           , rootNoteLabel
           , scaleLabel
           )


   #### Types to Extract:

     - `ScaleType` (Major | Pentatonic | Chromatic)
     - `RootNote` (C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B)


   #### Functions to Extract:

     - `getScalePattern : ScaleType -> List Int`
     - `getRootNoteOffset : RootNote -> Int`
     - `chromaticNoteNames : List String`
     - `notesPerOctave : ScaleType -> Int`


   #### New Functions to Add:

     - `scaleLabel : ScaleType -> String` (for UI)
     - `rootNoteLabel : RootNote -> String` (for UI)
     - `parseScaleType : String -> ScaleType` (with default)
     - `parseRootNote : String -> RootNote` (with default)
     - `allScaleTypes : List ScaleType`
     - `allRootNotes : List RootNote`


   ### Main2.elm Updates


   #### Import:

       import MusicTheory exposing (ScaleType, RootNote)


   #### Functions to Keep (Model-dependent):

     - `getTotalPitches : Model -> Int` - needs Model.scaleType and Model.octaveRange.count
     - `pitchIdxToMidi : Int -> Model -> Int` - needs Model context for calculations
     - `pitchIdxToNoteName : Int -> Model -> String` - needs Model context for calculations
     - All view functions (UI-specific)
     - All parsing functions in view selectors


   #### Update Function Calls:

   Replace direct calls with module-prefixed versions:

     - `getScalePattern model.scaleType` → `MusicTheory.getScalePattern model.scaleType`
     - `getRootNoteOffset model.rootNote` → `MusicTheory.getRootNoteOffset model.rootNote`
     - `notesPerOctave model.scaleType` → `MusicTheory.notesPerOctave model.scaleType`
     - Update view functions to use `MusicTheory.scaleLabel`, `MusicTheory.rootNoteLabel`
     - Update selectors to use `MusicTheory.allScaleTypes`, `MusicTheory.allRootNotes`


   #### Remove from Main2.elm:

     - `ScaleType`, `RootNote` type definitions
     - `getScalePattern`, `getRootNoteOffset`, `chromaticNoteNames`, `notesPerOctave` functions
     - Hardcoded scale/root note lists and parsing functions
     - Move `parseScaleType`, `parseRootNote` to MusicTheory.elm


   ## Implementation Steps

   1.  **Create MusicTheory.elm**:
         - Extract pure types and functions
         - Add missing label and parsing functions
         - Add allScaleTypes, allRootNotes lists
         - Test compilation

   2.  **Update Main2.elm imports**:
         - Add MusicTheory import with explicit types
         - Update all function calls to use MusicTheory prefix
         - Remove extracted type definitions and functions

   3.  **Update view functions**:
         - Replace hardcoded lists with MusicTheory.allScaleTypes/allRootNotes
         - Replace hardcoded case expressions with MusicTheory.scaleLabel/rootNoteLabel
         - Update parsing calls to use MusicTheory functions

   4.  **Test and refine**:
         - Ensure compilation succeeds
         - Verify UI still works correctly
         - Check that no music theory logic remains in Main2.elm


   ## Benefits

     - **Clean separation**: Music theory vs app-specific logic
     - **Reusability**: MusicTheory.elm can be used by other music applications
     - **Maintainability**: Easier to extend with new scales, modes, chord progressions
     - **Testability**: Pure functions are easy to unit test
     - **Consistency**: Follows same pattern as Instruments.elm module


   ## Module Boundaries

     - **MusicTheory.elm**: Pure music theory concepts, no Model dependencies
     - **Main2.elm**: App-specific pitch calculations that require Model context
     - **Instruments.elm**: Sound source definitions and instrument-related functions

-}


{- REFACTORING PLAN: Configurable Timing Structure (Bars/Beats/Split-beats)

   ## Goal

   Replace flat `totalSteps` with configurable timing structure based on musical bars, beats, and sub-beat divisions.

   ## Current Problem

   - `totalSteps = 32` is hardcoded and flat
   - No musical context (bars, beats, time signatures)
   - Difficult to align with standard musical notation
   - No visual grouping of beats/bars in UI

   ## Proposed Design

   ### New Types:

       type alias TimeSignature =
           { beatsPerBar : Int        -- 4 (for 4/4 time)
           , noteValue : NoteValue    -- Quarter (for 4/4 time)
           }

       type NoteValue = Whole | Half | Quarter | Eighth | Sixteenth

       type alias TimingConfig =
           { bars : Int               -- Number of bars (measures)
           , timeSignature : TimeSignature  -- Time signature
           , subdivisions : Int       -- Split-beats per beat (1, 2, 4, etc.)
           }

   ### Helper Functions:

       -- Calculate total steps from timing config
       totalStepsFromTiming : TimingConfig -> Int
       totalStepsFromTiming config =
           config.bars * config.timeSignature.beatsPerBar * config.subdivisions

       -- Convert absolute step index to bar/beat/subdivision
       stepToPosition : Int -> TimingConfig -> { bar : Int, beat : Int, subdivision : Int }

       -- Convert bar/beat/subdivision to absolute step index
       positionToStep : { bar : Int, beat : Int, subdivision : Int } -> TimingConfig -> Int

       -- Check if step is on beat boundary (for visual emphasis)
       isOnBeat : Int -> TimingConfig -> Bool

       -- Check if step is on bar boundary (for visual emphasis)
       isOnBar : Int -> TimingConfig -> Bool

   ## Implementation Plan

   ### Phase 1: Add TimingConfig to Model

   1. **Add new types and config to Model**:
       ```elm
       type alias Model =
           { -- ... existing fields
           , timingConfig : TimingConfig  -- Replace totalSteps
           -- ... rest
           }
       ```

   2. **Add helper function**:
       ```elm
       getTotalSteps : Model -> Int
       getTotalSteps model =
           totalStepsFromTiming model.timingConfig
       ```

   3. **Update all totalSteps references**:
       - `model.totalSteps` → `getTotalSteps model`
       - Keep existing logic working while adding new structure

   ### Phase 2: Enhance UI with Musical Context

   1. **Update grid rendering**:
       - Add bar separators (thicker borders every bar)
       - Add beat emphasis (subtle borders on beats)
       - Group step labels by bars/beats

   2. **Add timing controls in header**:
       ```elm
       viewTimingControls : Model -> Html Msg
       viewTimingControls model =
           div [ class "flex items-center gap-4" ]
               [ viewControlGroup "Bars" (viewBarsInput model.timingConfig.bars)
               , viewControlGroup "Beats" (viewTimeSignatureSelector model.timingConfig.timeSignature)
               , viewControlGroup "Subdivisions" (viewSubdivisionsInput model.timingConfig.subdivisions)
               ]
       ```

   3. **Update step labels to show bar.beat.subdivision**:
       ```elm
       viewStepLabel : Maybe Int -> Int -> TimingConfig -> Html Msg
       viewStepLabel currentStep stepIdx timingConfig =
           let
               position = stepToPosition stepIdx timingConfig
               labelText =
                   if timingConfig.subdivisions == 1 then
                       String.fromInt (position.bar + 1) ++ "." ++ String.fromInt (position.beat + 1)
                   else
                       String.fromInt (position.bar + 1) ++ "." ++ String.fromInt (position.beat + 1)
                       ++ "." ++ String.fromInt (position.subdivision + 1)
           ```

   ### Phase 3: Update Song Configuration

   1. **Replace totalSteps in SongConfig**:
       ```elm
       type alias SongConfig =
           { melody : List (List String)
           , percussion : List (List PercType)
           , timingConfig : TimingConfig  -- Replace totalSteps
           , bpm : Int
           , octaveRange : { start : Int, count : Int }
           }
       ```

   2. **Update twinkleSong example**:
       ```elm
       twinkleSong : SongConfig
       twinkleSong =
           { -- ... melody and percussion data (32 steps)
           , timingConfig =
               { bars = 8
               , timeSignature = { beatsPerBar = 4, noteValue = Quarter }
               , subdivisions = 1  -- 8 bars × 4 beats × 1 = 32 steps
               }
           , bpm = 180
           , octaveRange = { start = 3, count = 3 }
           }
       ```

   ### Phase 4: Enhanced Features (Future)

   1. **Visual improvements**:
       - Bar numbers in grid
       - Beat emphasis with different cell colors
       - Metronome click on beats vs subdivisions

   2. **Musical features**:
       - Different time signatures (3/4, 6/8, etc.)
       - Swing timing
       - Polyrhythms (different timing for different tracks)

   ## Migration Strategy

   ### Backward Compatibility:
   - Keep `getTotalSteps` function for existing code
   - Migrate incrementally (UI first, then internal logic)
   - Default timing config for existing songs

   ### Example Migration:
   ```elm
   -- Before:
   model.totalSteps

   -- After:
   getTotalSteps model

   -- Before:
   times (\stepIdx -> viewStepLabel currentStep stepIdx) model.totalSteps

   -- After:
   times (\stepIdx -> viewStepLabel currentStep stepIdx model.timingConfig) (getTotalSteps model)
   ```

   ## Benefits

   - **Musical context**: Aligns with standard musical notation and thinking
   - **Better UX**: Visual grouping of bars and beats makes patterns clearer
   - **Flexibility**: Easy to change song length, time signatures, subdivisions
   - **Future-proof**: Foundation for advanced rhythmic features
   - **Professional feel**: Standard music production interface conventions

   ## UI Mockup Concept

   ```
   [Bars: 8] [Time: 4/4] [Subdivisions: 1]

   Grid:
   Bar |  1.1 |  1.2 |  1.3 |  1.4 || 2.1 |  2.2 |  2.3 |  2.4 ||
   C4  | [●]  |      |  [●] |      || [●] |      |  [●] |      ||
   G3  |      |  [●] |      |  [●] ||     |  [●] |      |  [●] ||
   ```

   Double bars (||) between measures, single bars (|) between beats.

-}
