port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, footer, h1, header, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Patterns



-- PORTS


port playChord : { notes : List { note : Int, duration : Float, volume : Float }, when : Float } -> Cmd msg


port wakeAudioContext : () -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- CONSTANTS
-- Grid dimensions
-- MUSICAL CONFIGURATION
-- All music-related constants in one place
-- Grid octave configuration


octaveCount : Int
octaveCount =
    3


startingOctave : Int
startingOctave =
    3



-- MIDI reference point (standard)


midiC4 : Int
midiC4 =
    60



-- Major scale definition


majorScalePattern : List Int
majorScalePattern =
    [ 0, 2, 4, 5, 7, 9, 11 ]



-- Semitones from root: C D E F G A B


majorScaleNoteNames : List String
majorScaleNoteNames =
    [ "C", "D", "E", "F", "G", "A", "B" ]



-- Derived values


notesPerOctave : Int
notesPerOctave =
    List.length majorScaleNoteNames


noteCount : Int
noteCount =
    octaveCount * notesPerOctave


barCount : Int
barCount =
    16


beatsPerBar : Int
beatsPerBar =
    2


splitBeats : Int
splitBeats =
    2



-- Derived values


stepCount : Int
stepCount =
    barCount * beatsPerBar * splitBeats



-- Audio settings


bpm : Int
bpm =
    120


noteDuration : Float
noteDuration =
    (60.0 / toFloat bpm) / toFloat splitBeats


noteVolume : Float
noteVolume =
    0.8



-- MODEL


type PlayState
    = Stopped
    | Playing
        { startTime : Float
        , nextStepToSchedule : Int
        }


type alias Model =
    { grid : List (List Bool)
    , playState : PlayState
    , currentTime : Float
    , bpm : Int
    , barCount : Int
    , beatsPerBar : Int
    , splitBeats : Int
    , octaveStart : Int
    , octaveCount : Int
    }



-- MIDI Note Mapping
-- C4-centered system: C4 (MIDI 60) positioned at specific grid index
-- All other notes calculated relative to C4's position
-- SYSTEMATIC GRID-TO-MIDI CONVERSION
-- Clean functions without magic numbers
-- Convert grid index to musical position


type alias MusicalPosition =
    { octave : Int
    , noteIndex : Int -- 0-6 for C-B
    }


gridIndexToMusicalPosition : Int -> MusicalPosition
gridIndexToMusicalPosition gridIndex =
    let
        octaveOffset =
            gridIndex // notesPerOctave

        noteIndex =
            modBy notesPerOctave gridIndex

        octave =
            startingOctave + octaveOffset
    in
    { octave = octave, noteIndex = noteIndex }



-- Convert musical position to MIDI note


musicalPositionToMidi : MusicalPosition -> Int
musicalPositionToMidi position =
    let
        -- Octave offset from C4 reference point
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



-- Convert musical position to note label


musicalPositionToLabel : MusicalPosition -> String
musicalPositionToLabel position =
    let
        noteName =
            Maybe.withDefault "?" (List.drop position.noteIndex majorScaleNoteNames |> List.head)
    in
    noteName ++ String.fromInt position.octave



-- Main conversion functions (clean interface)


getMidiNoteForIndex : Int -> Int
getMidiNoteForIndex gridIndex =
    gridIndex
        |> gridIndexToMusicalPosition
        |> musicalPositionToMidi


getNoteLabelForIndex : Int -> String
getNoteLabelForIndex gridIndex =
    gridIndex
        |> gridIndexToMusicalPosition
        |> musicalPositionToLabel



-- Dynamic note list based on grid size and C4 position


noteList : List Int
noteList =
    List.range 0 (noteCount - 1)
        |> List.map getMidiNoteForIndex



-- Dynamic note labels based on C major scale


noteLabels : List String
noteLabels =
    List.range 0 (noteCount - 1)
        |> List.map getNoteLabelForIndex



-- Initialize empty grid


emptyGrid : Int -> Int -> List (List Bool)
emptyGrid noteCount_ stepCount_ =
    List.repeat noteCount_ (List.repeat stepCount_ False)



-- UPDATE


type Msg
    = ToggleCell Int Int -- noteIndex, stepIndex
    | Play
    | Stop
    | ClearGrid
    | TimeSync Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell noteIndex stepIndex ->
            -- TODO: Guard against invalid indices (noteIndex/stepIndex out of bounds)
            let
                wasActive =
                    getCellState noteIndex stepIndex model.grid

                newGrid =
                    toggleGridCell noteIndex stepIndex model.grid

                nowActive =
                    not wasActive

                playNoteCmd =
                    if nowActive then
                        let
                            midiNote =
                                getMidiNoteForIndex noteIndex

                            noteRecord =
                                { note = midiNote, duration = noteDuration, volume = noteVolume }
                        in
                        Cmd.batch [ wakeAudioContext (), playChord { notes = [ noteRecord ], when = model.currentTime } ]

                    else
                        Cmd.none
            in
            ( { model | grid = newGrid }, playNoteCmd )

        Play ->
            case model.playState of
                Stopped ->
                    ( { model | playState = Playing { startTime = model.currentTime, nextStepToSchedule = 0 } }
                    , wakeAudioContext ()
                    )

                Playing _ ->
                    -- Already playing, ignore duplicate Play message
                    ( model, Cmd.none )

        Stop ->
            case model.playState of
                Playing _ ->
                    ( { model | playState = Stopped }, Cmd.none )

                Stopped ->
                    -- Already stopped, ignore duplicate Stop message
                    ( model, Cmd.none )

        ClearGrid ->
            let
                noteCount_ =
                    model.octaveCount * 7

                stepCount_ =
                    model.barCount * model.beatsPerBar * model.splitBeats
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
                            floor (elapsedTime / noteDuration)

                        shouldSchedule =
                            absoluteStep >= nextStepToSchedule

                        stepTime =
                            startTime + toFloat nextStepToSchedule * noteDuration

                        gridStep =
                            modBy stepCount nextStepToSchedule

                        chordCmd =
                            if shouldSchedule then
                                let
                                    stepNotes =
                                        getActiveNotesForStep gridStep model.grid
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



-- GRID LOGIC


getActiveNotesForStep : Int -> List (List Bool) -> List { note : Int, duration : Float, volume : Float }
getActiveNotesForStep stepIndex grid =
    List.indexedMap
        (\noteIndex noteRow ->
            case List.drop stepIndex noteRow |> List.head of
                Just True ->
                    case List.drop noteIndex noteList |> List.head of
                        Just midiNote ->
                            Just { note = midiNote, duration = noteDuration, volume = noteVolume }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        )
        grid
        |> List.filterMap identity



-- Get current cell state


getCellState : Int -> Int -> List (List Bool) -> Bool
getCellState noteIndex stepIndex grid =
    case List.drop noteIndex grid |> List.head of
        Just noteRow ->
            case List.drop stepIndex noteRow |> List.head of
                Just isActive ->
                    isActive

                Nothing ->
                    False

        Nothing ->
            False


toggleGridCell : Int -> Int -> List (List Bool) -> List (List Bool)
toggleGridCell noteIndex stepIndex grid =
    List.indexedMap
        (\nIdx noteRow ->
            if nIdx == noteIndex then
                List.indexedMap
                    (\sIdx isActive ->
                        if sIdx == stepIndex then
                            not isActive

                        else
                            isActive
                    )
                    noteRow

            else
                noteRow
        )
        grid



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
    header [ class "bg-white shadow-sm border-b border-gray-200 px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ h1 [ class "text-2xl font-bold text-gray-800" ]
                [ text "Song Maker - Build 7" ]
            , div [ class "flex items-center gap-3" ]
                [ case model.playState of
                    Playing _ ->
                        button
                            [ class "bg-red-600 hover:brightness-110 text-white font-bold py-2 px-6 rounded-lg shadow-md transition-all"
                            , onClick Stop
                            ]
                            [ text "Stop" ]

                    Stopped ->
                        button
                            [ class "bg-green-600 hover:brightness-110 text-white font-bold py-2 px-6 rounded-lg shadow-md transition-all"
                            , onClick Play
                            ]
                            [ text "Play" ]
                , button
                    [ class "bg-gray-600 hover:brightness-110 text-white font-bold py-2 px-4 rounded-lg shadow-md transition-all"
                    , onClick ClearGrid
                    ]
                    [ text "Clear" ]
                ]
            , div [ class "text-sm text-gray-600" ]
                [ text (formatTime model.currentTime) ]
            ]
        ]


footerView : Model -> Html Msg
footerView _ =
    footer [ class "bg-white border-t border-gray-200 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-gray-600" ]
            [ div []
                [ text ("BPM: " ++ String.fromInt bpm) ]
            , div []
                [ text ("Grid: " ++ String.fromInt noteCount ++ " notes × " ++ String.fromInt stepCount ++ " steps | Bars: " ++ String.fromInt barCount ++ " | Beats/Bar: " ++ String.fromInt beatsPerBar ++ " | Splits: " ++ String.fromInt splitBeats) ]
            , div []
                [ text "Use mouse to toggle notes" ]
            ]
        ]


gridView : Model -> Html Msg
gridView model =
    div [ class "h-full overflow-auto p-6" ]
        [ div
            [ class "grid gap-1 w-fit mx-auto"
            , style "grid-template-columns" ("48px repeat(" ++ String.fromInt stepCount ++ ", 32px)")
            , style "grid-template-rows" ("repeat(" ++ String.fromInt (noteCount + 1) ++ ", 32px)")
            ]
            ([ div [ class "" ] [] -- Empty corner cell
             ]
                ++ List.indexedMap
                    (\stepIndex _ ->
                        div [ class "flex items-center justify-center text-xs font-bold text-gray-600" ]
                            [ text (String.fromInt (stepIndex + 1)) ]
                    )
                    (List.repeat stepCount ())
                ++ -- Note rows
                   (List.indexedMap
                        (\noteIndex noteRow ->
                            [ -- Note label
                              div [ class "flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200 rounded" ]
                                [ text (Maybe.withDefault "?" (List.drop noteIndex noteLabels |> List.head)) ]
                            ]
                                ++ List.indexedMap
                                    (\stepIndex isActive ->
                                        let
                                            cellClass =
                                                if isActive then
                                                    "bg-blue-600 hover:bg-blue-700 rounded cursor-pointer"

                                                else
                                                    "bg-gray-300 hover:bg-gray-400 rounded cursor-pointer"
                                        in
                                        div
                                            [ class cellClass
                                            , onClick (ToggleCell noteIndex stepIndex)
                                            ]
                                            []
                                    )
                                    noteRow
                        )
                        model.grid
                        |> List.concat
                   )
            )
        ]



-- MAIN


init : () -> ( Model, Cmd Msg )
init _ =
    let
        selectedPattern =
            Patterns.twinkleTwinkleChordsConfig
    in
    ( { grid = selectedPattern.grid
      , playState = Stopped
      , currentTime = 0.0
      , bpm = selectedPattern.bpm
      , barCount = selectedPattern.barCount
      , beatsPerBar = selectedPattern.beatsPerBar
      , splitBeats = selectedPattern.splitBeats
      , octaveStart = selectedPattern.octaveStart
      , octaveCount = selectedPattern.octaveCount
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
