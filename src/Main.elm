port module Main exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Patterns



-- PORTS


port playChord : { notes : List { note : Int, duration : Float, volume : Float }, when : Float } -> Cmd msg


port wakeAudioContext : () -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg


midiC4 : Int
midiC4 =
    60


majorScalePattern : List Int
majorScalePattern =
    [ 0, 2, 4, 5, 7, 9, 11 ]


majorScaleNoteNames : List String
majorScaleNoteNames =
    [ "C", "D", "E", "F", "G", "A", "B" ]


notesPerOctave : Int
notesPerOctave =
    List.length majorScaleNoteNames


totalSteps : { a | barCount : Int, beatsPerBar : Int, splitBeats : Int } -> Int
totalSteps record =
    record.barCount * record.beatsPerBar * record.splitBeats


totalNotes : { a | octaveCount : Int } -> Int
totalNotes record =
    record.octaveCount * notesPerOctave


noteDuration : { a | bpm : Int, splitBeats : Int } -> Float
noteDuration record =
    (60.0 / toFloat record.bpm) / toFloat record.splitBeats


noteLabels : { a | octaveCount : Int, startingOctave : Int } -> List String
noteLabels record =
    let
        noteCount_ =
            totalNotes record

        startingOctave_ =
            record.startingOctave
    in
    List.range 0 (noteCount_ - 1)
        |> List.map
            (\gridIndex ->
                let
                    octaveOffset =
                        gridIndex // notesPerOctave

                    noteIndex =
                        modBy notesPerOctave gridIndex

                    octave =
                        startingOctave_ + octaveOffset

                    noteName =
                        Maybe.withDefault "?" (List.drop noteIndex majorScaleNoteNames |> List.head)
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
    }


type alias MusicalPosition =
    { octave : Int
    , noteIndex : Int -- 0-6 for C-B
    }


gridIndexToMusicalPosition : Int -> { a | startingOctave : Int } -> MusicalPosition
gridIndexToMusicalPosition gridIndex record =
    let
        octaveOffset =
            gridIndex // notesPerOctave

        noteIndex =
            modBy notesPerOctave gridIndex

        octave =
            record.startingOctave + octaveOffset
    in
    { octave = octave, noteIndex = noteIndex }


musicalPositionToMidi : MusicalPosition -> Int
musicalPositionToMidi position =
    let
        octaveOffsetFromC4 =
            position.octave - 4

        -- Base MIDI for C in this octave
        baseMidiForC =
            midiC4 + (octaveOffsetFromC4 * 12)

        -- Semitone offset for this note in major scale
        semitoneOffset =
            Maybe.withDefault 0 (List.drop position.noteIndex majorScalePattern |> List.head)
    in
    baseMidiForC + semitoneOffset


getMidiNoteForIndex : Int -> { a | startingOctave : Int } -> Int
getMidiNoteForIndex gridIndex record =
    gridIndexToMusicalPosition gridIndex record
        |> musicalPositionToMidi


generateNoteList : { a | octaveCount : Int, startingOctave : Int } -> List Int
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
            -- TODO: Guard against time going backwards, NaN values, system clock adjustments
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
                    , chordCmd
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
                    ( setCellState noteIndex stepIndex True model, Cmd.none )

                Erasing ->
                    ( setCellState noteIndex stepIndex False model, Cmd.none )

                NotDrawing ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )


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


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-gray-50 flex flex-col" ]
        [ headerView model
        , div [ class "flex-1 overflow-hidden" ]
            [ gridView model ]
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


gridView : Model -> Html Msg
gridView model =
    let
        stepCount_ =
            totalSteps model

        noteCount_ =
            totalNotes model

        noteLabels_ =
            noteLabels model
    in
    div [ class "h-full overflow-auto" ]
        [ div
            [ class "grid gap-[1px]"
            , style "grid-template-columns" ("repeat(" ++ String.fromInt (stepCount_ + 1) ++ ", minmax(50px, 1fr))")
            , style "grid-template-rows" ("repeat(" ++ String.fromInt (noteCount_ + 1) ++ ", minmax(25px, 1fr))")
            , style "width" "max-content"
            , style "height" "max-content"
            , style "min-width" "100%"
            , style "min-height" "100%"
            ]
            ([ div [ class "" ] [] -- Empty corner cell
             ]
                ++ List.indexedMap
                    (\stepIndex _ ->
                        div [ class "flex items-center justify-center text-xs font-bold text-gray-600" ]
                            [ text (String.fromInt (stepIndex + 1)) ]
                    )
                    (List.repeat stepCount_ ())
                ++ -- Note rows
                   (List.range 0 (noteCount_ - 1)
                        |> List.map
                            (\noteIndex ->
                                [ -- Note label
                                  div [ class "flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200 rounded" ]
                                    [ text (Maybe.withDefault "?" (List.drop noteIndex noteLabels_ |> List.head)) ]
                                ]
                                    ++ (List.range 0 (stepCount_ - 1)
                                            |> List.map
                                                (\stepIndex ->
                                                    let
                                                        isActive =
                                                            getCellState noteIndex stepIndex model

                                                        cellClass =
                                                            if isActive then
                                                                "bg-blue-600 hover:bg-blue-700 rounded cursor-pointer"

                                                            else
                                                                "bg-gray-300 hover:bg-gray-400 rounded cursor-pointer"
                                                    in
                                                    div
                                                        [ class cellClass
                                                        , HE.onMouseDown (StartDrawing noteIndex stepIndex)
                                                        , HE.onMouseEnter (ContinueDrawing noteIndex stepIndex)
                                                        , HE.onMouseUp StopDrawing
                                                        ]
                                                        []
                                                )
                                       )
                            )
                        |> List.concat
                   )
            )
        ]



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
