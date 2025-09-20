port module Main exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Patterns
import Types exposing (RootNote(..), ScaleType(..))



-- PORTS


port playChord : { notes : List { note : Int, duration : Float, volume : Float }, when : Float } -> Cmd msg


port wakeAudioContext : () -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg


port scrollToActiveStep : { containerId : String, headerId : String } -> Cmd msg


midiC4 : Int
midiC4 =
    60


totalSteps : { a | barCount : Int, beatsPerBar : Int, splitBeats : Int } -> Int
totalSteps record =
    record.barCount * record.beatsPerBar * record.splitBeats


totalNotes : { a | octaveCount : Int, scaleType : ScaleType } -> Int
totalNotes record =
    record.octaveCount * Types.notesPerOctave record.scaleType


noteDuration : { a | bpm : Int, splitBeats : Int } -> Float
noteDuration record =
    (60.0 / toFloat record.bpm) / toFloat record.splitBeats


noteLabels : { a | octaveCount : Int, startingOctave : Int, scaleType : ScaleType, rootNote : RootNote } -> List String
noteLabels record =
    let
        noteCount_ =
            totalNotes record

        startingOctave_ =
            record.startingOctave

        scaleNoteNames =
            Types.getScaleNoteNames record.scaleType record.rootNote
    in
    List.range 0 (noteCount_ - 1)
        |> List.map
            (\gridIndex ->
                let
                    octaveOffset =
                        gridIndex // Types.notesPerOctave record.scaleType

                    noteIndex =
                        modBy (Types.notesPerOctave record.scaleType) gridIndex

                    octave =
                        startingOctave_ + octaveOffset

                    noteName =
                        Maybe.withDefault "?" (List.drop noteIndex scaleNoteNames |> List.head)
                in
                noteName ++ String.fromInt octave
            )



-- MODEL


type PlayState
    = Stopped
    | Playing
        { startTime : Float
        , nextStepToSchedule : Int
        }


type DrawState
    = NotDrawing
    | Drawing
    | Erasing


type alias Model =
    { grid : List (List Bool)
    , playState : PlayState
    , currentTime : Float
    , bpm : Int
    , barCount : Int
    , beatsPerBar : Int
    , splitBeats : Int
    , startingOctave : Int
    , octaveCount : Int
    , noteVolume : Float
    , selectedPatternIndex : Int
    , drawState : DrawState
    , scaleType : ScaleType
    , rootNote : RootNote
    }


type alias MusicalPosition =
    { octave : Int
    , noteIndex : Int -- 0-6 for C-B
    }


gridIndexToMusicalPosition : Int -> { a | startingOctave : Int, scaleType : ScaleType } -> MusicalPosition
gridIndexToMusicalPosition gridIndex record =
    let
        octaveOffset =
            gridIndex // Types.notesPerOctave record.scaleType

        noteIndex =
            modBy (Types.notesPerOctave record.scaleType) gridIndex

        octave =
            record.startingOctave + octaveOffset
    in
    { octave = octave, noteIndex = noteIndex }


musicalPositionToMidi : MusicalPosition -> { a | scaleType : ScaleType, rootNote : RootNote } -> Int
musicalPositionToMidi position record =
    let
        octaveOffsetFromC4 =
            position.octave - 4

        -- Base MIDI for C in this octave
        baseMidiForC =
            midiC4 + (octaveOffsetFromC4 * 12)

        -- Root note offset
        rootOffset =
            Types.getRootNoteOffset record.rootNote

        -- Semitone offset for this note in the selected scale
        scalePattern =
            Types.getScalePattern record.scaleType

        semitoneOffset =
            Maybe.withDefault 0 (List.drop position.noteIndex scalePattern |> List.head)
    in
    baseMidiForC + rootOffset + semitoneOffset


getMidiNoteForIndex : Int -> { a | startingOctave : Int, scaleType : ScaleType, rootNote : RootNote } -> Int
getMidiNoteForIndex gridIndex record =
    gridIndexToMusicalPosition gridIndex record
        |> (\position -> musicalPositionToMidi position record)


generateNoteList : { a | octaveCount : Int, startingOctave : Int, scaleType : ScaleType, rootNote : RootNote } -> List Int
generateNoteList record =
    let
        noteCount_ =
            totalNotes record
    in
    List.range 0 (noteCount_ - 1)
        |> List.map (\gridIndex -> getMidiNoteForIndex gridIndex record)


emptyGrid : Int -> Int -> List (List Bool)
emptyGrid noteCount_ stepCount_ =
    List.repeat noteCount_ (List.repeat stepCount_ False)


resizeGrid : Int -> Int -> List (List Bool) -> List (List Bool)
resizeGrid newNoteCount newStepCount existingGrid =
    let
        -- Helper to resize a single note row (horizontal resize)
        resizeNoteRow : List Bool -> List Bool
        resizeNoteRow existingRow =
            let
                currentLength =
                    List.length existingRow

                truncatedRow =
                    List.take newStepCount existingRow

                paddedRow =
                    if currentLength < newStepCount then
                        truncatedRow ++ List.repeat (newStepCount - currentLength) False

                    else
                        truncatedRow
            in
            paddedRow

        -- Resize existing note rows
        resizedExistingRows =
            List.map resizeNoteRow existingGrid

        -- Take only the rows we need (vertical resize - truncate)
        truncatedRows =
            List.take newNoteCount resizedExistingRows

        -- Add new empty rows if we need more notes (vertical resize - pad)
        currentNoteCount =
            List.length truncatedRows

        newEmptyRows =
            if currentNoteCount < newNoteCount then
                List.repeat (newNoteCount - currentNoteCount) (List.repeat newStepCount False)

            else
                []

        finalGrid =
            truncatedRows ++ newEmptyRows
    in
    finalGrid


getAllPatterns : List Patterns.PatternConfig
getAllPatterns =
    [ Patterns.twinkleTwinkleConfig
    , Patterns.twinkleTwinkleChordsConfig
    , Patterns.twinkleTwinkleChordsV2Config
    , Patterns.twinkleTwinkleChordsV3Config
    , Patterns.vShapeConfig
    ]


getPatternByIndex : Int -> Patterns.PatternConfig
getPatternByIndex index =
    List.drop index getAllPatterns
        |> List.head
        |> Maybe.withDefault Patterns.twinkleTwinkleChordsV3Config



-- UPDATE


type Msg
    = Play
    | Stop
    | ClearGrid
    | TimeSync Float
    | ChangePattern Int
    | ChangeScaleType ScaleType
    | ChangeRootNote RootNote
    | ChangeOctaveCount Int
    | ChangeStartingOctave Int
    | ChangeBarCount Int
    | ChangeBeatsPerBar Int
    | ChangeSplitBeats Int
    | StartDrawing Int Int -- noteIndex, stepIndex
    | ContinueDrawing Int Int -- noteIndex, stepIndex
    | StopDrawing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            case model.playState of
                Stopped ->
                    ( { model | playState = Playing { startTime = model.currentTime, nextStepToSchedule = 0 } }
                    , wakeAudioContext ()
                    )

                Playing _ ->
                    ( model, Cmd.none )

        Stop ->
            case model.playState of
                Playing _ ->
                    ( { model | playState = Stopped }, Cmd.none )

                Stopped ->
                    ( model, Cmd.none )

        ClearGrid ->
            let
                noteCount_ =
                    totalNotes model

                stepCount_ =
                    totalSteps model
            in
            ( { model | grid = emptyGrid noteCount_ stepCount_ }, Cmd.none )

        TimeSync currentTime ->
            case model.playState of
                Stopped ->
                    ( { model | currentTime = currentTime }, Cmd.none )

                Playing { startTime, nextStepToSchedule } ->
                    let
                        elapsedTime =
                            currentTime - startTime

                        absoluteStep =
                            floor (elapsedTime / noteDuration model)

                        shouldSchedule =
                            absoluteStep >= nextStepToSchedule

                        stepTime =
                            startTime + toFloat nextStepToSchedule * noteDuration model

                        gridStep =
                            modBy (totalSteps model) nextStepToSchedule

                        chordCmd =
                            if shouldSchedule then
                                let
                                    stepNotes =
                                        getActiveNotesForStep gridStep model
                                in
                                if not (List.isEmpty stepNotes) then
                                    playChord { notes = stepNotes, when = stepTime }

                                else
                                    Cmd.none

                            else
                                Cmd.none

                        scrollCmd =
                            if shouldSchedule then
                                --scrollToActiveStep { containerId = "grid-container", headerId = "active-step-header" }
                                Cmd.none

                            else
                                Cmd.none
                    in
                    ( { model
                        | currentTime = currentTime
                        , playState =
                            Playing
                                { startTime = startTime
                                , nextStepToSchedule =
                                    if shouldSchedule then
                                        nextStepToSchedule + 1

                                    else
                                        nextStepToSchedule
                                }
                      }
                    , Cmd.batch [ chordCmd, scrollCmd ]
                    )

        ChangePattern newIndex ->
            let
                newPattern =
                    getPatternByIndex newIndex

                ( playState, cmd ) =
                    case model.playState of
                        Playing _ ->
                            ( Playing { startTime = model.currentTime, nextStepToSchedule = 0 }, wakeAudioContext () )

                        Stopped ->
                            ( Stopped, Cmd.none )
            in
            ( { model
                | selectedPatternIndex = newIndex
                , grid = newPattern.grid
                , bpm = newPattern.bpm
                , barCount = newPattern.barCount
                , beatsPerBar = newPattern.beatsPerBar
                , splitBeats = newPattern.splitBeats
                , startingOctave = newPattern.octaveStart
                , octaveCount = newPattern.octaveCount
                , scaleType = newPattern.scaleType
                , rootNote = newPattern.rootNote
                , playState = playState
                , drawState = NotDrawing
              }
            , cmd
            )

        StartDrawing noteIndex stepIndex ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentState =
                            getCellState noteIndex stepIndex model

                        newDrawState =
                            if currentState then
                                Erasing

                            else
                                Drawing

                        updatedModel =
                            setCellState noteIndex stepIndex (not currentState) model

                        playNoteCmd =
                            if not currentState then
                                let
                                    midiNote =
                                        getMidiNoteForIndex noteIndex model

                                    noteRecord =
                                        { note = midiNote, duration = noteDuration model, volume = model.noteVolume }
                                in
                                Cmd.batch [ wakeAudioContext (), playChord { notes = [ noteRecord ], when = model.currentTime } ]

                            else
                                Cmd.none
                    in
                    ( { updatedModel | drawState = newDrawState }, playNoteCmd )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawing noteIndex stepIndex ->
            case model.drawState of
                Drawing ->
                    let
                        updatedModel =
                            setCellState noteIndex stepIndex True model

                        midiNote =
                            getMidiNoteForIndex noteIndex model

                        noteRecord =
                            { note = midiNote, duration = noteDuration model, volume = model.noteVolume }

                        playNoteCmd =
                            Cmd.batch [ wakeAudioContext (), playChord { notes = [ noteRecord ], when = model.currentTime } ]
                    in
                    ( updatedModel, playNoteCmd )

                Erasing ->
                    ( setCellState noteIndex stepIndex False model, Cmd.none )

                NotDrawing ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        ChangeScaleType newScaleType ->
            let
                noteCount_ =
                    totalNotes { model | scaleType = newScaleType }

                stepCount_ =
                    totalSteps model

                newGrid =
                    resizeGrid noteCount_ stepCount_ model.grid
            in
            ( { model | scaleType = newScaleType, grid = newGrid }, Cmd.none )

        ChangeRootNote newRootNote ->
            ( { model | rootNote = newRootNote }, Cmd.none )

        ChangeOctaveCount newOctaveCount ->
            let
                clampedOctaveCount =
                    max 1 (min 8 newOctaveCount)

                noteCount_ =
                    totalNotes { model | octaveCount = clampedOctaveCount }

                stepCount_ =
                    totalSteps model

                newGrid =
                    resizeGrid noteCount_ stepCount_ model.grid
            in
            ( { model | octaveCount = clampedOctaveCount, grid = newGrid }, Cmd.none )

        ChangeStartingOctave newStartingOctave ->
            let
                clampedStartingOctave =
                    max 1 (min 8 newStartingOctave)
            in
            ( { model | startingOctave = clampedStartingOctave }, Cmd.none )

        ChangeBarCount newBarCount ->
            let
                clampedBarCount =
                    max 1 (min 16 newBarCount)

                noteCount_ =
                    totalNotes model

                stepCount_ =
                    totalSteps { model | barCount = clampedBarCount }

                newGrid =
                    resizeGrid noteCount_ stepCount_ model.grid
            in
            ( { model | barCount = clampedBarCount, grid = newGrid }, Cmd.none )

        ChangeBeatsPerBar newBeatsPerBar ->
            let
                clampedBeatsPerBar =
                    max 1 (min 16 newBeatsPerBar)

                noteCount_ =
                    totalNotes model

                stepCount_ =
                    totalSteps { model | beatsPerBar = clampedBeatsPerBar }

                newGrid =
                    resizeGrid noteCount_ stepCount_ model.grid
            in
            ( { model | beatsPerBar = clampedBeatsPerBar, grid = newGrid }, Cmd.none )

        ChangeSplitBeats newSplitBeats ->
            let
                clampedSplitBeats =
                    max 1 (min 8 newSplitBeats)

                noteCount_ =
                    totalNotes model

                stepCount_ =
                    totalSteps { model | splitBeats = clampedSplitBeats }

                newGrid =
                    resizeGrid noteCount_ stepCount_ model.grid
            in
            ( { model | splitBeats = clampedSplitBeats, grid = newGrid }, Cmd.none )


getActiveNotesForStep : Int -> Model -> List { note : Int, duration : Float, volume : Float }
getActiveNotesForStep stepIndex model =
    let
        noteList_ =
            generateNoteList model

        duration =
            noteDuration model

        noteCount_ =
            totalNotes model
    in
    List.range 0 (noteCount_ - 1)
        |> List.map
            (\noteIndex ->
                if getCellState noteIndex stepIndex model then
                    case List.drop noteIndex noteList_ |> List.head of
                        Just midiNote ->
                            Just { note = midiNote, duration = duration, volume = model.noteVolume }

                        Nothing ->
                            Nothing

                else
                    Nothing
            )
        |> List.filterMap identity


getCellState : Int -> Int -> Model -> Bool
getCellState noteIndex stepIndex model =
    let
        noteCount_ =
            totalNotes model

        stepCount_ =
            totalSteps model

        isWithinBounds =
            noteIndex >= 0 && noteIndex < noteCount_ && stepIndex >= 0 && stepIndex < stepCount_
    in
    if isWithinBounds then
        case List.drop noteIndex model.grid |> List.head of
            Just noteRow ->
                case List.drop stepIndex noteRow |> List.head of
                    Just isActive ->
                        isActive

                    Nothing ->
                        False

            Nothing ->
                False

    else
        False


setCellState : Int -> Int -> Bool -> Model -> Model
setCellState noteIndex stepIndex isActive model =
    let
        noteCount_ =
            totalNotes model

        stepCount_ =
            totalSteps model

        isWithinBounds =
            noteIndex >= 0 && noteIndex < noteCount_ && stepIndex >= 0 && stepIndex < stepCount_
    in
    if isWithinBounds then
        let
            newGrid =
                List.indexedMap
                    (\nIdx noteRow ->
                        if nIdx == noteIndex then
                            List.indexedMap
                                (\sIdx currentState ->
                                    if sIdx == stepIndex then
                                        isActive

                                    else
                                        currentState
                                )
                                noteRow

                        else
                            noteRow
                    )
                    model.grid
        in
        { model | grid = newGrid }

    else
        model



-- VIEW


formatTime : Float -> String
formatTime seconds =
    let
        -- Round to 1 decimal place
        roundedSeconds =
            toFloat (round (seconds * 10)) / 10

        minutes =
            floor (roundedSeconds / 60)

        remainingSecs =
            roundedSeconds - (toFloat minutes * 60)

        -- Separate whole seconds and decimal part
        wholeSeconds =
            floor remainingSecs

        decimalPart =
            round ((remainingSecs - toFloat wholeSeconds) * 10)

        -- Format with proper padding
        secondsString =
            if wholeSeconds < 10 then
                "0" ++ String.fromInt wholeSeconds

            else
                String.fromInt wholeSeconds

        formattedTime =
            String.fromInt minutes ++ ":" ++ secondsString ++ "." ++ String.fromInt decimalPart
    in
    formattedTime


numberInput : String -> Int -> (Int -> Msg) -> Html Msg
numberInput label value onChange =
    div [ class "flex items-center gap-1" ]
        [ H.label [ class "text-xs font-medium text-gray-600" ] [ text label ]
        , H.input
            [ HA.type_ "number"
            , HA.value (String.fromInt value)
            , class "w-16 px-2 py-1 text-xs border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500"
            , HE.onInput (\str -> Maybe.withDefault value (String.toInt str) |> onChange)
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-gray-50 flex flex-col select-none" ]
        [ headerView model
        , div [ class "flex-1 overflow-auto" ]
            [ viewGrid model ]
        , footerView model
        ]


headerView : Model -> Html Msg
headerView model =
    H.header [ class "bg-white shadow-sm border-b border-gray-200 px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ H.h1 [ class "text-2xl font-bold text-gray-800" ]
                [ text "Song Maker - Build 7" ]
            , div [ class "flex items-center gap-3" ]
                [ H.select
                    [ class "bg-white border border-gray-300 rounded-lg px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                    , HE.onInput
                        (\indexStr ->
                            case String.toInt indexStr of
                                Just index ->
                                    ChangePattern index

                                Nothing ->
                                    ChangePattern 0
                        )
                    ]
                    (List.indexedMap
                        (\index pattern ->
                            H.option
                                [ HA.value (String.fromInt index)
                                , HA.selected (index == model.selectedPatternIndex)
                                ]
                                [ text pattern.name ]
                        )
                        getAllPatterns
                    )
                , H.select
                    [ class "bg-white border border-gray-300 rounded-lg px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                    , HE.onInput (Types.stringToScaleType >> ChangeScaleType)
                    ]
                    (List.map
                        (\scaleType ->
                            H.option
                                [ HA.value (Types.scaleTypeToString scaleType)
                                , HA.selected (scaleType == model.scaleType)
                                ]
                                [ text (Types.scaleTypeToString scaleType) ]
                        )
                        Types.allScaleTypes
                    )
                , H.select
                    [ class "bg-white border border-gray-300 rounded-lg px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                    , HE.onInput (Types.stringToRootNote >> ChangeRootNote)
                    ]
                    (List.map
                        (\rootNote ->
                            H.option
                                [ HA.value (Types.rootNoteToString rootNote)
                                , HA.selected (rootNote == model.rootNote)
                                ]
                                [ text (Types.rootNoteToString rootNote) ]
                        )
                        Types.allRootNotes
                    )
                , numberInput "Start" model.startingOctave ChangeStartingOctave
                , numberInput "Oct" model.octaveCount ChangeOctaveCount
                , numberInput "Bars" model.barCount ChangeBarCount
                , numberInput "Beats" model.beatsPerBar ChangeBeatsPerBar
                , numberInput "Split" model.splitBeats ChangeSplitBeats
                , case model.playState of
                    Playing _ ->
                        H.button
                            [ class "bg-red-600 hover:brightness-110 text-white font-bold py-2 px-6 rounded-lg shadow-md transition-all"
                            , HE.onClick Stop
                            ]
                            [ text "Stop" ]

                    Stopped ->
                        H.button
                            [ class "bg-green-600 hover:brightness-110 text-white font-bold py-2 px-6 rounded-lg shadow-md transition-all"
                            , HE.onClick Play
                            ]
                            [ text "Play" ]
                , H.button
                    [ class "bg-gray-600 hover:brightness-110 text-white font-bold py-2 px-4 rounded-lg shadow-md transition-all"
                    , HE.onClick ClearGrid
                    ]
                    [ text "Clear" ]
                ]
            , div [ class "text-sm text-gray-600" ]
                [ text (formatTime model.currentTime) ]
            ]
        ]


footerView : Model -> Html Msg
footerView model =
    H.footer [ class "bg-white border-t border-gray-200 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-gray-600" ]
            [ div []
                [ text ("BPM: " ++ String.fromInt model.bpm) ]
            , div []
                [ text ("Grid: " ++ String.fromInt (totalNotes model) ++ " notes × " ++ String.fromInt (totalSteps model) ++ " steps | Bars: " ++ String.fromInt model.barCount ++ " | Beats/Bar: " ++ String.fromInt model.beatsPerBar ++ " | Splits: " ++ String.fromInt model.splitBeats) ]
            , div []
                [ text "Use mouse to toggle notes" ]
            ]
        ]


getCurrentPlayingStep : Model -> Maybe Int
getCurrentPlayingStep model =
    case model.playState of
        Playing { startTime, nextStepToSchedule } ->
            if model.currentTime == startTime then
                -- since we use modulo for looping, on play start very first step should be 0
                -- modulo approach would give us last step instead
                Just 0

            else
                -- using nextStepToSchedule - 1 to get currently playing step
                Just (modBy (totalSteps model) (nextStepToSchedule - 1))

        Stopped ->
            Nothing


viewNoteLabel : List String -> Int -> Html msg
viewNoteLabel noteLabels_ noteIndex =
    div [ class "flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200" ]
        [ text (Maybe.withDefault "?" (List.drop noteIndex noteLabels_ |> List.head)) ]


getBorderClasses : Int -> Model -> String
getBorderClasses stepIndex model =
    let
        stepsPerBeat =
            model.splitBeats

        stepsPerBar =
            model.beatsPerBar * model.splitBeats

        isBarStart =
            modBy stepsPerBar stepIndex == 0 && stepIndex > 0

        isBeatStart =
            modBy stepsPerBeat stepIndex == 0 && stepIndex > 0 && not isBarStart
    in
    if isBarStart then
        "border-l-2 border-gray-800"

    else if isBeatStart then
        "border-l border-gray-600"

    else
        ""


viewGrid : Model -> Html Msg
viewGrid model =
    let
        stepCount_ =
            totalSteps model

        noteCount_ =
            totalNotes model

        noteLabels_ =
            noteLabels model

        currentStep =
            getCurrentPlayingStep model
    in
    div
        [ class "grid p-1 overflow-x-auto w-screen max-w-full"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt (stepCount_ + 1) ++ ", minmax(48px, 1fr))")
        , style "grid-template-rows" ("repeat(" ++ String.fromInt (noteCount_ + 1) ++ ", minmax(24px, 1fr))")
        , style "width" "max-content"
        , style "height" "max-content"
        , style "min-width" "100%"
        , style "min-height" "100%"
        , HA.id "grid-container"
        ]
        ([ div [ class "" ] [] -- Empty corner cell
         ]
            ++ List.indexedMap
                (\stepIndex _ ->
                    let
                        isCurrentStep =
                            currentStep == Just stepIndex

                        borderClasses =
                            getBorderClasses stepIndex model

                        stepHeaderClass =
                            if isCurrentStep then
                                "flex items-center justify-center text-xs font-bold text-gray-700 bg-blue-200 " ++ borderClasses

                            else
                                "flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200 " ++ borderClasses

                        stepHeaderAttrs =
                            if isCurrentStep then
                                [ HA.id "active-step-header" ]

                            else
                                []
                    in
                    div ([ class stepHeaderClass ] ++ stepHeaderAttrs)
                        [ text (String.fromInt (stepIndex + 1)) ]
                )
                (List.repeat stepCount_ ())
            ++ -- Note rows
               (List.range 0 (noteCount_ - 1)
                    |> List.map
                        (\noteIndex ->
                            [ viewNoteLabel noteLabels_ noteIndex ]
                                ++ (List.range 0 (stepCount_ - 1)
                                        |> List.map (viewGridCell currentStep model noteIndex)
                                   )
                        )
                    |> List.concat
               )
        )


viewGridCell : Maybe Int -> Model -> Int -> Int -> Html Msg
viewGridCell currentStep model noteIndex stepIndex =
    let
        isActive =
            getCellState noteIndex stepIndex model

        isCurrentStep =
            case currentStep of
                Just idx ->
                    idx == stepIndex

                Nothing ->
                    False

        cellClass =
            case ( isActive, isCurrentStep ) of
                ( True, True ) ->
                    "bg-blue-600 hover:bg-blue-700 just-played"

                ( True, False ) ->
                    "bg-blue-600 hover:bg-blue-700"

                ( False, True ) ->
                    "bg-blue-200 hover:bg-blue-300"

                ( False, False ) ->
                    "bg-blue-100 hover:bg-blue-200"
    in
    div
        [ class cellClass
        , class (getBorderClasses stepIndex model)
        , class "cursor-pointer"
        , HE.onMouseDown (StartDrawing noteIndex stepIndex)
        , HE.onMouseEnter (ContinueDrawing noteIndex stepIndex)
        , HE.onMouseUp StopDrawing
        ]
        []



-- MAIN


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultPatternIndex =
            3

        selectedPattern =
            getPatternByIndex defaultPatternIndex
    in
    ( { grid = selectedPattern.grid
      , playState = Stopped
      , currentTime = 0.0
      , bpm = selectedPattern.bpm
      , barCount = selectedPattern.barCount
      , beatsPerBar = selectedPattern.beatsPerBar
      , splitBeats = selectedPattern.splitBeats
      , startingOctave = selectedPattern.octaveStart
      , octaveCount = selectedPattern.octaveCount
      , noteVolume = 0.8
      , selectedPatternIndex = defaultPatternIndex
      , drawState = NotDrawing
      , scaleType = selectedPattern.scaleType
      , rootNote = selectedPattern.rootNote
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    timeSync TimeSync


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
