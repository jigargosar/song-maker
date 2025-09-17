port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, footer, h1, h2, header, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)



-- PORTS


port playChord : List { note : Int, duration : Float, volume : Float } -> Cmd msg


port wakeAudioContext : () -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- CONSTANTS
-- Grid dimensions


-- Number of octaves to display
octaveCount : Int
octaveCount =
    1


-- C major scale notes per octave (7 notes)
notesPerOctave : Int
notesPerOctave =
    7


-- Total number of notes
noteCount : Int
noteCount =
    octaveCount * notesPerOctave


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


-- C major scale MIDI offsets from C (white keys only)
cMajorScaleMidiOffsets : List Int
cMajorScaleMidiOffsets =
    [ 0, 2, 4, 5, 7, 9, 11 ] -- C D E F G A B


-- C major scale note names
cMajorScaleNames : List String
cMajorScaleNames =
    [ "C", "D", "E", "F", "G", "A", "B" ]


-- Starting octave (top of grid)
startingOctave : Int
startingOctave =
    4


-- Generate MIDI note for given grid index
getMidiNoteForIndex : Int -> Int
getMidiNoteForIndex gridIndex =
    let
        -- Calculate which octave and note within octave
        octaveOffset =
            gridIndex // notesPerOctave

        noteInOctave =
            modBy notesPerOctave gridIndex

        -- Current octave (going down from starting octave)
        currentOctave =
            startingOctave - octaveOffset

        -- Get MIDI offset for this note in C major scale
        midiOffset =
            Maybe.withDefault 0 (List.drop noteInOctave cMajorScaleMidiOffsets |> List.head)

        -- Calculate base MIDI note for C in this octave
        baseMidiForOctave =
            (currentOctave + 1) * 12
    in
    baseMidiForOctave + midiOffset


-- Generate note label for grid index
getNoteLabelForIndex : Int -> String
getNoteLabelForIndex gridIndex =
    let
        octaveOffset =
            gridIndex // notesPerOctave

        noteInOctave =
            modBy notesPerOctave gridIndex

        currentOctave =
            startingOctave - octaveOffset

        noteName =
            Maybe.withDefault "?" (List.drop noteInOctave cMajorScaleNames |> List.head)
    in
    noteName ++ String.fromInt currentOctave


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
                newGrid =
                    toggleGridCell noteIndex beatIndex model.grid
            in
            ( { model | grid = newGrid }, Cmd.none )

        Play ->
            case model.playState of
                Stopped ->
                    let
                        activeNotes =
                            getActiveNotesForBeat 0 model.grid

                        chordCmd =
                            if List.isEmpty activeNotes then
                                Cmd.none

                            else
                                playChord activeNotes
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

                        chordCmd =
                            if beatChanged && not (List.isEmpty activeNotes) then
                                playChord activeNotes

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
        { init = \_ -> ( { grid = emptyGrid, playState = Stopped, currentTime = 0.0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }
