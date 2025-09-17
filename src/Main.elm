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


type PlayState
    = Stopped
    | Playing
        { startTime : Float
        , currentBeat : Int
        }


type alias Model =
    { grid : List (List Bool)  -- 8 notes × 8 beats, [note][beat]
    , playState : PlayState
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


-- Demo melody pattern - simple Twinkle Twinkle Little Star
demoGrid : List (List Bool)
demoGrid =
    [ -- G4: beats 1,2,5,6 (Twinkle twinkle, little star)
      [ True, True, False, False, True, True, False, False ]
    , -- F4: empty
      [ False, False, False, False, False, False, False, False ]
    , -- E4: empty
      [ False, False, False, False, False, False, False, False ]
    , -- D4: beats 3,4,7,8 (how I wonder what you are)
      [ False, False, True, True, False, False, True, True ]
    , -- C4: beat 8 (are)
      [ False, False, False, False, False, False, False, True ]
    , -- B3: empty
      [ False, False, False, False, False, False, False, False ]
    , -- A3: empty
      [ False, False, False, False, False, False, False, False ]
    , -- G3: empty
      [ False, False, False, False, False, False, False, False ]
    ]


-- UPDATE


type Msg
    = ToggleCell Int Int  -- noteIndex, beatIndex
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
                newGrid = toggleGridCell noteIndex beatIndex model.grid
            in
            ( { model | grid = newGrid }, Cmd.none )

        Play ->
            case model.playState of
                Stopped ->
                    let
                        activeNotes = getActiveNotesForBeat 0 model.grid
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
                        beatDuration = 0.5  -- 120 BPM = 0.5 seconds per beat
                        elapsedTime = currentTime - startTime
                        expectedBeat = modBy 8 (floor (elapsedTime / beatDuration))

                        beatChanged = expectedBeat /= currentBeat

                        activeNotes = getActiveNotesForBeat expectedBeat model.grid
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
        [ -- Single line header with title, controls, and time
          div [ class "flex items-center justify-between mb-6" ]
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
        { init = \_ -> ( { grid = demoGrid, playState = Stopped, currentTime = 0.0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }