port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


-- PORTS


port playChord : List { note : Int, duration : Float, volume : Float } -> Cmd msg


port wakeAudioContext : () -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg


-- MODEL


type alias Model =
    { grid : List (List Bool)  -- 8 notes × 8 beats, [note][beat]
    , isPlaying : Bool
    , currentBeat : Int
    , currentTime : Float
    }


-- Notes: C4 to G4 (8 notes, high to low for display)
noteList : List Int
noteList = [ 67, 65, 64, 62, 60, 59, 57, 55 ]  -- G4, F4, E4, D4, C4, B3, A3, G3


noteLabels : List String
noteLabels = [ "G4", "F4", "E4", "D4", "C4", "B3", "A3", "G3" ]


-- Initialize empty 8x8 grid
emptyGrid : List (List Bool)
emptyGrid = List.repeat 8 (List.repeat 8 False)


-- UPDATE


type Msg
    = ToggleCell Int Int  -- noteIndex, beatIndex
    | Play
    | Stop
    | TimeSync Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCell noteIndex beatIndex ->
            let
                newGrid = toggleGridCell noteIndex beatIndex model.grid
            in
            ( { model | grid = newGrid }, Cmd.none )

        Play ->
            let
                activeNotes = getActiveNotesForBeat 0 model.grid
                chordCmd =
                    if List.isEmpty activeNotes then
                        Cmd.none
                    else
                        playChord activeNotes
            in
            ( { model | isPlaying = True, currentBeat = 0 }, Cmd.batch [ wakeAudioContext (), chordCmd ] )

        Stop ->
            ( { model | isPlaying = False }, Cmd.none )

        TimeSync currentTime ->
            if model.isPlaying then
                let
                    -- Calculate beat based on time (120 BPM = 0.5 seconds per beat)
                    beatDuration = 0.5
                    newBeat = modBy 8 (floor (currentTime / beatDuration))

                    beatChanged = newBeat /= model.currentBeat

                    activeNotes = getActiveNotesForBeat newBeat model.grid
                    chordCmd =
                        if beatChanged && not (List.isEmpty activeNotes) then
                            playChord activeNotes
                        else
                            Cmd.none
                in
                ( { model | currentTime = currentTime, currentBeat = newBeat }, chordCmd )
            else
                ( { model | currentTime = currentTime }, Cmd.none )


-- GRID LOGIC


getActiveNotesForBeat : Int -> List (List Bool) -> List { note : Int, duration : Float, volume : Float }
getActiveNotesForBeat beatIndex grid =
    List.indexedMap (\noteIndex noteRow ->
        case List.drop beatIndex noteRow |> List.head of
            Just True ->
                case List.drop noteIndex noteList |> List.head of
                    Just midiNote ->
                        Just { note = midiNote, duration = 0.4, volume = 0.8 }
                    Nothing ->
                        Nothing
            _ ->
                Nothing
    ) grid
        |> List.filterMap identity


toggleGridCell : Int -> Int -> List (List Bool) -> List (List Bool)
toggleGridCell noteIndex beatIndex grid =
    List.indexedMap (\nIdx noteRow ->
        if nIdx == noteIndex then
            List.indexedMap (\bIdx isActive ->
                if bIdx == beatIndex then
                    not isActive
                else
                    isActive
            ) noteRow
        else
            noteRow
    ) grid


-- VIEW


formatTime : Float -> String
formatTime seconds =
    let
        -- Round to 1 decimal place
        roundedSeconds = toFloat (round (seconds * 10)) / 10
        minutes = floor (roundedSeconds / 60)
        remainingSecs = roundedSeconds - (toFloat minutes * 60)

        -- Separate whole seconds and decimal part
        wholeSeconds = floor remainingSecs
        decimalPart = round ((remainingSecs - toFloat wholeSeconds) * 10)

        -- Format with proper padding
        secsStr =
            if wholeSeconds < 10 then
                "0" ++ String.fromInt wholeSeconds
            else
                String.fromInt wholeSeconds

        formattedTime = String.fromInt minutes ++ ":" ++ secsStr ++ "." ++ String.fromInt decimalPart
    in
    formattedTime


view : Model -> Html Msg
view model =
    div [ class "p-6 bg-gray-50 min-h-screen" ]
        [ h1 [ class "text-3xl font-bold mb-6 text-gray-800 text-center" ]
            [ text "Song Maker" ]

        , div [ class "text-center mb-4 text-sm text-gray-600" ]
            [ text ("Audio Time: " ++ formatTime model.currentTime) ]

        -- ========== 2D GRID SEQUENCER ==========
        , div [ class "border-t-2 border-gray-300 pt-4 mt-4" ] []
        , h2 [ class "text-xl font-bold text-center mb-4 text-gray-800" ] [ text "🎵 Grid Sequencer" ]

        , div [ class "text-center mb-4" ]
            [ if model.isPlaying then
                button
                    [ class "bg-red-600 hover:brightness-110 text-white font-bold py-3 px-8 rounded-lg shadow-md transition-all"
                    , onClick Stop
                    ]
                    [ text "⏹ Stop" ]
              else
                button
                    [ class "bg-green-600 hover:brightness-110 text-white font-bold py-3 px-8 rounded-lg shadow-md transition-all"
                    , onClick Play
                    ]
                    [ text "▶ Play" ]
            ]

        , gridView model
        ]


gridView : Model -> Html Msg
gridView model =
    div [ class "overflow-x-auto" ]
        [ div [ class "grid grid-cols-9 gap-1 w-fit mx-auto" ]
            ([ div [ class "w-12 h-8" ] []  -- Empty corner cell
             ] ++
             -- Beat numbers header
             List.indexedMap (\beatIndex _ ->
                 div [ class "w-8 h-8 flex items-center justify-center text-xs font-bold text-gray-600" ]
                     [ text (String.fromInt (beatIndex + 1)) ]
             ) (List.repeat 8 ()) ++
             -- Note rows
             (List.indexedMap (\noteIndex noteRow ->
                 [ -- Note label
                   div [ class "w-12 h-8 flex items-center justify-center text-xs font-bold text-gray-700 bg-gray-200 rounded" ]
                       [ text (Maybe.withDefault "?" (List.drop noteIndex noteLabels |> List.head)) ]
                 ] ++
                 -- Beat cells for this note
                 List.indexedMap (\beatIndex isActive ->
                     let
                         cellClass =
                             if isActive then
                                 "w-8 h-8 bg-blue-600 hover:bg-blue-700 rounded cursor-pointer"
                             else
                                 "w-8 h-8 bg-gray-300 hover:bg-gray-400 rounded cursor-pointer"
                     in
                     div
                         [ class cellClass
                         , onClick (ToggleCell noteIndex beatIndex)
                         ]
                         []
                 ) noteRow
             ) model.grid |> List.concat)
            )
        ]


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { grid = emptyGrid, isPlaying = False, currentBeat = 0, currentTime = 0.0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }