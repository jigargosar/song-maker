port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, footer, h1, h2, header, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



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


endingOctave : Int
endingOctave =
    startingOctave - octaveCount + 1



-- Musical structure (like Chrome Song Maker)


barCount : Int
barCount =
    4


beatsPerBar : Int
beatsPerBar =
    2


splitBeats : Int
splitBeats =
    2



-- Derived values


beatCount : Int
beatCount =
    barCount * beatsPerBar * splitBeats



-- Audio settings


defaultBpm : Int
defaultBpm =
    120


defaultNoteDuration : Float
defaultNoteDuration =
    0.4


defaultNoteVolume : Float
defaultNoteVolume =
    0.8



-- Derived values


beatDurationSeconds : Float
beatDurationSeconds =
    60.0 / toFloat defaultBpm


subdivisionDurationSeconds : Float
subdivisionDurationSeconds =
    beatDurationSeconds / toFloat splitBeats



-- 120 BPM = 0.5 seconds per beat


gridColumns : Int
gridColumns =
    beatCount + 1



-- beats + note label column
-- MODEL


type PlayState
    = Stopped
    | Playing
        { startTime : Float
        , currentBeat : Int
        }


type alias Model =
    { grid : List (List Bool) -- noteCount × beatCount, [note][beat]
    , playState : PlayState
    , currentTime : Float
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


emptyGrid : List (List Bool)
emptyGrid =
    List.repeat noteCount (List.repeat beatCount False)



-- V-shaped melody demo: smooth stepwise progression C3 -> C4 -> C3
vShapeGrid : List (List Bool)
vShapeGrid =
    emptyGrid
        -- Ascending: C3 to C4 (beats 1-8)
        |> toggleGridCell 0 0   -- C3 at beat 1
        |> toggleGridCell 1 1   -- D3 at beat 2
        |> toggleGridCell 2 2   -- E3 at beat 3
        |> toggleGridCell 3 3   -- F3 at beat 4
        |> toggleGridCell 4 4   -- G3 at beat 5
        |> toggleGridCell 5 5   -- A3 at beat 6
        |> toggleGridCell 6 6   -- B3 at beat 7
        |> toggleGridCell 7 7   -- C4 at beat 8
        -- Peak: hold C4 for 2 beats (beats 8-9)
        |> toggleGridCell 7 8   -- C4 at beat 9 (held)
        -- Descending: C4 back to C3 (beats 10-16)
        |> toggleGridCell 6 9   -- B3 at beat 10
        |> toggleGridCell 5 10  -- A3 at beat 11
        |> toggleGridCell 4 11  -- G3 at beat 12
        |> toggleGridCell 3 12  -- F3 at beat 13
        |> toggleGridCell 2 13  -- E3 at beat 14
        |> toggleGridCell 1 14  -- D3 at beat 15
        |> toggleGridCell 0 15  -- C3 at beat 16


-- Empty demo grid - no preset melody


demoGrid : List (List Bool)
demoGrid =
    emptyGrid



-- UPDATE


type Msg
    = ToggleCell Int Int -- noteIndex, beatIndex
    | Play
    | Stop
    | ClearGrid
    | TimeSync Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell noteIndex beatIndex ->
            -- TODO: Guard against invalid indices (noteIndex/beatIndex out of bounds)
            let
                wasActive =
                    getCellState noteIndex beatIndex model.grid

                newGrid =
                    toggleGridCell noteIndex beatIndex model.grid

                nowActive =
                    not wasActive

                playNoteCmd =
                    if nowActive then
                        let
                            midiNote =
                                getMidiNoteForIndex noteIndex

                            noteRecord =
                                { note = midiNote, duration = defaultNoteDuration, volume = defaultNoteVolume }
                        in
                        Cmd.batch [ wakeAudioContext (), playChord { notes = [ noteRecord ], when = model.currentTime } ]

                    else
                        Cmd.none
            in
            ( { model | grid = newGrid }, playNoteCmd )

        Play ->
            case model.playState of
                Stopped ->
                    let
                        -- Play beat 0 immediately
                        activeNotesBeat0 =
                            getActiveNotesForBeat 0 model.grid

                        -- Schedule beat 1 for proper timing
                        activeNotesBeat1 =
                            getActiveNotesForBeat 1 model.grid

                        beat1Time =
                            model.currentTime + beatDurationSeconds

                        chordCmd =
                            Cmd.batch
                                [ -- Play beat 0 now
                                  if List.isEmpty activeNotesBeat0 then
                                    Cmd.none
                                  else
                                    playChord { notes = activeNotesBeat0, when = model.currentTime }
                                , -- Schedule beat 1 at the right time
                                  if List.isEmpty activeNotesBeat1 then
                                    Cmd.none
                                  else
                                    playChord { notes = activeNotesBeat1, when = beat1Time }
                                ]
                    in
                    ( { model | playState = Playing { startTime = model.currentTime, currentBeat = 0 } }
                    , Cmd.batch [ wakeAudioContext (), chordCmd ]
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
            ( { model | grid = emptyGrid }, Cmd.none )

        TimeSync currentTime ->
            -- TODO: Guard against time going backwards, NaN values, system clock adjustments
            case model.playState of
                Stopped ->
                    ( { model | currentTime = currentTime }, Cmd.none )

                Playing { startTime, currentBeat } ->
                    let
                        elapsedTime =
                            currentTime - startTime

                        expectedBeat =
                            modBy beatCount (floor (elapsedTime / beatDurationSeconds))

                        beatChanged =
                            expectedBeat /= currentBeat

                        activeNotes =
                            getActiveNotesForBeat expectedBeat model.grid

                        -- Simple: schedule next beat when current beat changes
                        shouldSchedule =
                            expectedBeat /= currentBeat

                        nextBeat =
                            modBy beatCount (expectedBeat + 1)

                        -- Calculate which loop the next beat will be in
                        currentLoopNumber =
                            floor (elapsedTime / (toFloat beatCount * beatDurationSeconds))

                        nextBeatLoopNumber =
                            if nextBeat == 0 then
                                currentLoopNumber + 1  -- Next beat wraps to new loop
                            else
                                currentLoopNumber       -- Next beat is in same loop

                        nextBeatAbsoluteTime =
                            startTime + (toFloat nextBeatLoopNumber * toFloat beatCount * beatDurationSeconds) + (toFloat nextBeat * beatDurationSeconds)

                        nextBeatNotes =
                            getActiveNotesForBeat nextBeat model.grid

                        chordCmd =
                            if shouldSchedule && not (List.isEmpty nextBeatNotes) then
                                playChord { notes = nextBeatNotes, when = nextBeatAbsoluteTime }

                            else
                                Cmd.none
                    in
                    ( { model
                        | currentTime = currentTime
                        , playState = Playing { startTime = startTime, currentBeat = expectedBeat }
                      }
                    , chordCmd
                    )



-- GRID LOGIC


getActiveNotesForBeat : Int -> List (List Bool) -> List { note : Int, duration : Float, volume : Float }
getActiveNotesForBeat beatIndex grid =
    List.indexedMap
        (\noteIndex noteRow ->
            case List.drop beatIndex noteRow |> List.head of
                Just True ->
                    case List.drop noteIndex noteList |> List.head of
                        Just midiNote ->
                            Just { note = midiNote, duration = defaultNoteDuration, volume = defaultNoteVolume }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        )
        grid
        |> List.filterMap identity


-- Get current cell state
getCellState : Int -> Int -> List (List Bool) -> Bool
getCellState noteIndex beatIndex grid =
    case List.drop noteIndex grid |> List.head of
        Just noteRow ->
            case List.drop beatIndex noteRow |> List.head of
                Just isActive ->
                    isActive

                Nothing ->
                    False

        Nothing ->
            False


toggleGridCell : Int -> Int -> List (List Bool) -> List (List Bool)
toggleGridCell noteIndex beatIndex grid =
    List.indexedMap
        (\nIdx noteRow ->
            if nIdx == noteIndex then
                List.indexedMap
                    (\bIdx isActive ->
                        if bIdx == beatIndex then
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
        secsStr =
            if wholeSeconds < 10 then
                "0" ++ String.fromInt wholeSeconds

            else
                String.fromInt wholeSeconds

        formattedTime =
            String.fromInt minutes ++ ":" ++ secsStr ++ "." ++ String.fromInt decimalPart
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
                [ text "Song Maker" ]
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
footerView model =
    footer [ class "bg-white border-t border-gray-200 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-gray-600" ]
            [ div []
                [ text ("BPM: " ++ String.fromInt defaultBpm) ]
            , div []
                [ text ("Grid: " ++ String.fromInt noteCount ++ " notes × " ++ String.fromInt beatCount ++ " beats | Bars: " ++ String.fromInt barCount ++ " | Beats/Bar: " ++ String.fromInt beatsPerBar ++ " | Splits: " ++ String.fromInt splitBeats) ]
            , div []
                [ text "Use mouse to toggle notes" ]
            ]
        ]


gridView : Model -> Html Msg
gridView model =
    div [ class "h-full overflow-auto p-6" ]
        [ div
            [ class "grid gap-1 w-fit mx-auto"
            , style "grid-template-columns" ("48px repeat(" ++ String.fromInt beatCount ++ ", 32px)")
            , style "grid-template-rows" ("repeat(" ++ String.fromInt (noteCount + 1) ++ ", 32px)")
            ]
            ([ div [ class "" ] [] -- Empty corner cell
             ]
                ++ -- Beat numbers header
                   List.indexedMap
                    (\beatIndex _ ->
                        div [ class "flex items-center justify-center text-xs font-bold text-gray-600" ]
                            [ text (String.fromInt (beatIndex + 1)) ]
                    )
                    (List.repeat beatCount ())
                ++ -- Note rows
                   (List.indexedMap
                        (\noteIndex noteRow ->
                            [ -- Note label
                              div [ class "flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200 rounded" ]
                                [ text (Maybe.withDefault "?" (List.drop noteIndex noteLabels |> List.head)) ]
                            ]
                                ++ -- Beat cells for this note
                                   List.indexedMap
                                    (\beatIndex isActive ->
                                        let
                                            cellClass =
                                                if isActive then
                                                    "bg-blue-600 hover:bg-blue-700 rounded cursor-pointer"

                                                else
                                                    "bg-gray-300 hover:bg-gray-400 rounded cursor-pointer"
                                        in
                                        div
                                            [ class cellClass
                                            , onClick (ToggleCell noteIndex beatIndex)
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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { grid = vShapeGrid, playState = Stopped, currentTime = 0.0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }
