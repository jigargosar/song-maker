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


noteCount : Int
noteCount =
    24


beatCount : Int
beatCount =
    8



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
    60.0 / toFloat defaultBpm / 2.0



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



-- Notes: 3 octaves from B5 to C3 (24 notes, high to low for display)


noteList : List Int
noteList =
    [ 83
    , 81
    , 79
    , 77
    , 76
    , 74
    , 72
    , 71
    , 69
    , 67
    , 65
    , 64 -- B5 to C5
    , 62
    , 60
    , 59
    , 57
    , 55
    , 53
    , 52
    , 50
    , 48
    , 47
    , 45
    , 43 -- B4 to C4, B3 to C3
    ]


noteLabels : List String
noteLabels =
    [ "B5"
    , "A5"
    , "G5"
    , "F5"
    , "E5"
    , "D5"
    , "C5"
    , "B4"
    , "A4"
    , "G4"
    , "F4"
    , "E4"
    , "D4"
    , "C4"
    , "B3"
    , "A3"
    , "G3"
    , "F3"
    , "E3"
    , "D3"
    , "C3"
    , "B2"
    , "A2"
    , "G2"
    ]



-- Initialize empty grid


emptyGrid : List (List Bool)
emptyGrid =
    List.repeat noteCount (List.repeat beatCount False)



-- Demo melody pattern - simple Twinkle Twinkle Little Star


demoGrid : List (List Bool)
demoGrid =
    let
        emptyRow =
            List.repeat beatCount False
    in
    [ emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow -- B5 to G4 (indices 0-8)
    , [ True, True, False, False, True, True, False, False ] -- G4: index 9, beats 1,2,5,6 (Twinkle twinkle, little star)
    , emptyRow -- F4: index 10
    , emptyRow -- E4: index 11
    , [ False, False, True, True, False, False, True, False ] -- D4: index 12, beats 3,4,7 (how I wonder what you)
    , [ False, False, False, False, False, False, False, True ] -- C4: index 13, beat 8 (are)
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow
    , emptyRow -- B3 to A2 (indices 14-23)
    ]



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
                [ text ("Notes: " ++ String.fromInt noteCount ++ " | Beats: " ++ String.fromInt beatCount) ]
            , div []
                [ text "Use mouse to toggle notes" ]
            ]
        ]


gridView : Model -> Html Msg
gridView model =
    div [ class "h-full overflow-auto p-6" ]
        [ div
            [ class "grid gap-1 w-fit mx-auto"
            , style "grid-template-columns" "48px repeat(8, 32px)"
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
        { init = \_ -> ( { grid = demoGrid, playState = Stopped, currentTime = 0.0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }
