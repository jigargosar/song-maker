port module Main2 exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
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


getTotalTonees : Model -> Int
getTotalTonees model =
    notesPerOctave model.scaleType * model.octaveRange.count


toneIdxToMidi : Int -> Model -> Int
toneIdxToMidi toneIdx model =
    let
        scalePattern =
            getScalePattern model.scaleType

        rootOffset =
            getRootNoteOffset model.rootNote

        notesInScale =
            notesPerOctave model.scaleType

        octaveIdx =
            toneIdx // notesInScale

        noteIdx =
            modBy notesInScale toneIdx

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


toneIdxToNoteName : Int -> Model -> String
toneIdxToNoteName toneIdx model =
    let
        scalePattern =
            getScalePattern model.scaleType

        rootOffset =
            getRootNoteOffset model.rootNote

        notesInScale =
            notesPerOctave model.scaleType

        octaveIdx =
            toneIdx // notesInScale

        noteIdx =
            modBy notesInScale toneIdx

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


toneInstrumentName : ToneInstrument -> String
toneInstrumentName instrument =
    case instrument of
        GrandPianoSBLive ->
            "_tone_0000_SBLive_sf2"

        MarimbaSBLLive ->
            "_tone_0120_SBLive_sf2"


drumKitConfig : DrumKit -> { kickInstrument : String, kickMidi : Int, snareInstrument : String, snareMidi : Int }
drumKitConfig kit =
    case kit of
        StandardKit ->
            { kickInstrument = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareInstrument = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }

        RockKit ->
            { kickInstrument = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareInstrument = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }



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


type alias TonePos =
    { toneIdx : Int, stepIdx : Int }


type alias ToneGrid =
    Set ( Int, Int )


type ToneInstrument
    = GrandPianoSBLive
    | MarimbaSBLLive


type DrumKit
    = StandardKit
    | RockKit


type PercType
    = Kick
    | Snare


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
    | DrawingTone
    | ErasingTone
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
    , toneGrid : ToneGrid
    , percGrid : PercGrid
    , drawState : DrawState
    , playState : PlayState
    , audioContextTime : Float
    , bpm : Int
    , currentToneInstrument : ToneInstrument
    , currentDrumKit : DrumKit
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { scaleType = Major
      , rootNote = C
      , octaveRange = { start = 3, count = 3 }
      , totalSteps = 32
      , toneGrid = Set.empty
      , percGrid = Set.empty
      , drawState = NotDrawing
      , playState = Stopped
      , audioContextTime = 0.0
      , bpm = 120
      , currentToneInstrument = GrandPianoSBLive
      , currentDrumKit = StandardKit
      }
        |> applySong twinkleSong
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartDrawingTone TonePos
    | ContinueDrawingTone TonePos
    | StopDrawing
    | StartDrawingPerc PercPos
    | ContinueDrawingPerc PercPos
    | PlayToneNote Int
    | PlayPercNote PercType
    | Play
    | Stop
    | TimeSync Float
    | ChangeScaleType ScaleType
    | ChangeRootNote RootNote
    | ChangeOctaveStart Int
    | ChangeOctaveCount Int
    | ChangeBPM Int
    | ChangeToneInstrument ToneInstrument
    | ChangeDrumKit DrumKit


subscriptions : Model -> Sub Msg
subscriptions _ =
    timeSync TimeSync


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawingTone position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isToneCellActive position model.toneGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingTone

                            else
                                DrawingTone
                    in
                    ( { model
                        | drawState = newDrawState
                        , toneGrid = updateToneCell position (not currentlyActive) model.toneGrid
                      }
                    , playToneCmdIf (not currentlyActive) position.toneIdx model
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingTone position ->
            case model.drawState of
                DrawingTone ->
                    ( { model | toneGrid = updateToneCell position True model.toneGrid }
                    , playToneCmdIf True position.toneIdx model
                    )

                ErasingTone ->
                    ( { model | toneGrid = updateToneCell position False model.toneGrid }, Cmd.none )

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

        PlayToneNote toneIdx ->
            ( model, playToneCmdIf True toneIdx model )

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

                newToneGrid =
                    resizeToneGrid model newModel model.toneGrid
            in
            ( { newModel | toneGrid = newToneGrid }
            , Cmd.none
            )

        ChangeRootNote newRootNote ->
            let
                newModel =
                    { model | rootNote = newRootNote }

                newToneGrid =
                    resizeToneGrid model newModel model.toneGrid
            in
            ( { newModel | toneGrid = newToneGrid }
            , Cmd.none
            )

        ChangeOctaveStart newStart ->
            let
                clampedStart =
                    max 1 newStart

                newModel =
                    { model | octaveRange = { start = clampedStart, count = model.octaveRange.count } }

                newToneGrid =
                    resizeToneGrid model newModel model.toneGrid
            in
            ( { newModel | toneGrid = newToneGrid }
            , Cmd.none
            )

        ChangeOctaveCount newCount ->
            let
                clampedCount =
                    max 1 newCount

                newModel =
                    { model | octaveRange = { start = model.octaveRange.start, count = clampedCount } }

                newToneGrid =
                    resizeToneGrid model newModel model.toneGrid
            in
            ( { newModel | toneGrid = newToneGrid }
            , Cmd.none
            )

        ChangeBPM newBPM ->
            ( { model | bpm = max 1 newBPM }
            , Cmd.none
            )

        ChangeToneInstrument newInstrument ->
            ( { model | currentToneInstrument = newInstrument }
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
            format "minmax($toneLabelColMinWidth, auto) repeat($totalSteps, minmax($stepColMinWidth, 1fr))"
                [ ( "$toneLabelColMinWidth", px 48 )
                , ( "$totalSteps", String.fromInt model.totalSteps )
                , ( "$stepColMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalTonees, minmax($toneRowMinHeight, 1fr)) repeat(2, $percRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalTonees", String.fromInt (getTotalTonees model) )
                , ( "$toneRowMinHeight", px 32 )
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
            ++ {- Tone rows -} (times (\toneIdx -> viewToneRow model model.toneGrid currentStep toneIdx) (getTotalTonees model) |> List.concat)
            ++ {- Perc Snare row -} viewPercRow Snare model.totalSteps model.percGrid currentStep
            ++ {- Perc Kick row -} viewPercRow Kick model.totalSteps model.percGrid currentStep
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


viewToneRow : Model -> ToneGrid -> Maybe Int -> Int -> List (Html Msg)
viewToneRow model toneGrid currentStep toneIdx =
    let
        viewToneLabel =
            div
                [ class labelBgColorAndClass, class "border-[0.5px]" ]
                [ text (toneIdxToNoteName toneIdx model) ]
    in
    -- TODO: Should we fix function parameters?
    viewToneLabel :: times (\stepIdx -> viewToneCell toneIdx toneGrid currentStep stepIdx) model.totalSteps


viewToneCell : Int -> ToneGrid -> Maybe Int -> Int -> Html Msg
viewToneCell toneIdx toneGrid currentStep stepIdx =
    let
        position =
            { toneIdx = toneIdx, stepIdx = stepIdx }

        isActive =
            isToneCellActive position toneGrid

        isCurrentStep =
            currentStep == Just stepIdx

        -- TODO: try refactor
        noteClass =
            if isActive then
                toneCellColor toneIdx

            else if isCurrentStep then
                "bg-gray-700 hover:bg-gray-600"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class noteClass
        , class "border-[0.5px] border-gray-600 cursor-pointer "
        , HE.onMouseDown (StartDrawingTone position)
        , HE.onMouseEnter (ContinueDrawingTone position)
        , HE.onMouseUp StopDrawing
        ]
        []


viewPercRow : PercType -> Int -> PercGrid -> Maybe Int -> List (Html Msg)
viewPercRow percType totalSteps percGrid currentStep =
    let
        percTypeName =
            case percType of
                Snare ->
                    "Snare"

                Kick ->
                    "Kick"

        stickyClass =
            case percType of
                Snare ->
                    "sticky bottom-12 h-12 z-10 border-t-3"

                Kick ->
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
                Snare ->
                    "sticky bottom-12 h-12 z-10  border-t-3"

                Kick ->
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


toPercRowIdx : PercType -> Int
toPercRowIdx percType =
    case percType of
        Snare ->
            0

        Kick ->
            1


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( toPercRowIdx percType, stepIdx )



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

        toneNotes =
            times
                (\toneIdx ->
                    let
                        position =
                            { toneIdx = toneIdx, stepIdx = stepIdx }
                    in
                    if isToneCellActive position model.toneGrid then
                        Just
                            { instrument = toneInstrumentName model.currentToneInstrument
                            , midi = toneIdxToMidi toneIdx model
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                (getTotalTonees model)
                |> List.filterMap identity

        drumConfig =
            drumKitConfig model.currentDrumKit

        percNotes =
            [ Kick, Snare ]
                |> List.filterMap
                    (\percType ->
                        let
                            position =
                                { percType = percType, stepIdx = stepIdx }

                            ( instrumentName, midiNote ) =
                                case percType of
                                    Kick ->
                                        ( drumConfig.kickInstrument, drumConfig.kickMidi )

                                    Snare ->
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
    toneNotes ++ percNotes



-- Audio Helper Functions


playToneCmdIf : Bool -> Int -> Model -> Cmd Msg
playToneCmdIf shouldPlay toneIdx model =
    if shouldPlay then
        playNote
            { instrument = toneInstrumentName model.currentToneInstrument
            , midi = toneIdxToMidi toneIdx model
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
                drumKitConfig model.currentDrumKit

            ( instrumentName, midiNote ) =
                case percType of
                    Kick ->
                        ( drumConfig.kickInstrument, drumConfig.kickMidi )

                    Snare ->
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


resizeToneGrid : Model -> Model -> ToneGrid -> ToneGrid
resizeToneGrid oldModel newModel existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( toneIdx, stepIdx ) ->
                let
                    midiTone =
                        toneIdxToMidi toneIdx oldModel

                    newToneIdx =
                        midiToToneIdx midiTone newModel
                in
                if newToneIdx >= 0 && newToneIdx < getTotalTonees newModel && stepIdx < newModel.totalSteps then
                    Just ( newToneIdx, stepIdx )

                else
                    Nothing
            )
        |> Set.fromList


midiToToneIdx : Int -> Model -> Int
midiToToneIdx targetMidi model =
    let
        totalTonees =
            getTotalTonees model
    in
    List.range 0 (totalTonees - 1)
        |> List.filter (\toneIdx -> toneIdxToMidi toneIdx model == targetMidi)
        |> List.head
        |> Maybe.withDefault -1


noteNameToToneIdx : String -> Model -> Int
noteNameToToneIdx noteName model =
    let
        totalTonees =
            getTotalTonees model
    in
    List.range 0 (totalTonees - 1)
        |> List.filter (\toneIdx -> toneIdxToNoteName toneIdx model == noteName)
        |> List.head
        |> Maybe.withDefault -1


twinkleSong : SongConfig
twinkleSong =
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
        [ [ Kick ], [], [ Snare ], [] ]
            -- "how I wonder what"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "what you are so"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "far above the world"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "Up above the world"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "so high like a"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "diamond in the"
            ++ [ [ Kick ], [], [ Snare ], [] ]
            -- "sky (end)"
            ++ [ [ Kick ], [], [ Snare ], [] ]
    , totalSteps = 32
    , bpm = 180
    , octaveRange = { start = 3, count = 3 }
    }


applySong : SongConfig -> Model -> Model
applySong songConfig model =
    let
        toneGrid =
            convertMelodyToGrid songConfig.melody model

        percGrid =
            convertPercussionToGrid songConfig.percussion
    in
    { model
        | toneGrid = toneGrid
        , percGrid = percGrid
        , totalSteps = songConfig.totalSteps
        , bpm = songConfig.bpm
        , octaveRange = songConfig.octaveRange
    }


convertMelodyToGrid : List (List String) -> Model -> ToneGrid
convertMelodyToGrid stepMelodies model =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        let
                            toneIdx =
                                noteNameToToneIdx noteName model
                        in
                        if toneIdx >= 0 then
                            Just ( toneIdx, stepIdx )

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
                        ( toPercRowIdx percType, stepIdx )
                    )
                    percTypes
            )
        |> List.concat
        |> Set.fromList



-- Cell State Management


isToneCellActive : TonePos -> ToneGrid -> Bool
isToneCellActive { toneIdx, stepIdx } toneGrid =
    Set.member ( toneIdx, stepIdx ) toneGrid


updateToneCell : TonePos -> Bool -> ToneGrid -> ToneGrid
updateToneCell { toneIdx, stepIdx } isActive toneGrid =
    if isActive then
        Set.insert ( toneIdx, stepIdx ) toneGrid

    else
        Set.remove ( toneIdx, stepIdx ) toneGrid


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
            Kick ->
                -- Circle symbol
                div [ class "w-6 h-6 rounded-full", class accentBgColor ] []

            Snare ->
                -- Triangle symbol
                div [ class "w-6 h-6", class accentBgColor, style "clip-path" "polygon(50% 0%, 0% 100%, 100% 100%)" ] []

    else
        -- Small dim dot for inactive
        div [ class "w-1.5 h-1.5 bg-gray-500 rounded-full" ] []


{-| TODO: why mod 7 and how it works
it works for major and pentatonic, since we are doing mod by7
but doesnt work for chromatic
-}
toneCellColor : Int -> String
toneCellColor toneIdx =
    case modBy 7 toneIdx of
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
        [ viewControlGroup "Tone" (viewToneInstrumentSelector model.currentToneInstrument)
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
        , HE.onInput (parseScaleType >> Maybe.withDefault Major >> ChangeScaleType)
        ]
        [ H.option [ HA.value "Major", HA.selected (currentScale == Major) ] [ text "Major" ]
        , H.option [ HA.value "Pentatonic", HA.selected (currentScale == Pentatonic) ] [ text "Pentatonic" ]
        , H.option [ HA.value "Chromatic", HA.selected (currentScale == Chromatic) ] [ text "Chromatic" ]
        ]


viewRootNoteSelector : RootNote -> Html Msg
viewRootNoteSelector currentRoot =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (parseRootNote >> Maybe.withDefault C >> ChangeRootNote)
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


parseScaleType : String -> Maybe ScaleType
parseScaleType str =
    case str of
        "Major" ->
            Just Major

        "Pentatonic" ->
            Just Pentatonic

        "Chromatic" ->
            Just Chromatic

        _ ->
            Nothing


parseRootNote : String -> Maybe RootNote
parseRootNote str =
    case str of
        "C" ->
            Just C

        "C#" ->
            Just CSharp

        "D" ->
            Just D

        "D#" ->
            Just DSharp

        "E" ->
            Just E

        "F" ->
            Just F

        "F#" ->
            Just FSharp

        "G" ->
            Just G

        "G#" ->
            Just GSharp

        "A" ->
            Just A

        "A#" ->
            Just ASharp

        "B" ->
            Just B

        _ ->
            Nothing


viewToneInstrumentSelector : ToneInstrument -> Html Msg
viewToneInstrumentSelector currentInstrument =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (parseToneInstrument >> ChangeToneInstrument)
        ]
        (List.map (viewToneInstrumentOption currentInstrument) allToneInstruments)


viewDrumKitSelector : DrumKit -> Html Msg
viewDrumKitSelector currentDrumKit =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (parseDrumKit >> ChangeDrumKit)
        ]
        (List.map (viewDrumKitOption currentDrumKit) allDrumKits)


viewToneInstrumentOption : ToneInstrument -> ToneInstrument -> Html Msg
viewToneInstrumentOption currentInstrument instrument =
    H.option
        [ HA.value (toneInstrumentToString instrument)
        , HA.selected (currentInstrument == instrument)
        ]
        [ text (toneInstrumentToString instrument) ]


viewDrumKitOption : DrumKit -> DrumKit -> Html Msg
viewDrumKitOption currentDrumKit drumKit =
    H.option
        [ HA.value (drumKitToString drumKit)
        , HA.selected (currentDrumKit == drumKit)
        ]
        [ text (drumKitToString drumKit) ]


allToneInstruments : List ToneInstrument
allToneInstruments =
    [ GrandPianoSBLive, MarimbaSBLLive ]


allDrumKits : List DrumKit
allDrumKits =
    [ StandardKit, RockKit ]


toneInstrumentToString : ToneInstrument -> String
toneInstrumentToString instrument =
    case instrument of
        GrandPianoSBLive ->
            "Piano"

        MarimbaSBLLive ->
            "Marimba"


drumKitToString : DrumKit -> String
drumKitToString drumKit =
    case drumKit of
        StandardKit ->
            "Standard"

        RockKit ->
            "Rock"


parseToneInstrument : String -> ToneInstrument
parseToneInstrument str =
    case str of
        "Piano" ->
            GrandPianoSBLive

        "Marimba" ->
            MarimbaSBLLive

        _ ->
            GrandPianoSBLive


parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Standard" ->
            StandardKit

        "Rock" ->
            RockKit

        _ ->
            StandardKit



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
