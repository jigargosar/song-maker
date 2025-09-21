port module Main2 exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Set exposing (Set)



-- PORTS


port playNote : { note : Int, duration : Float, volume : Float } -> Cmd msg


port playPercussion : { note : Int, duration : Float, volume : Float } -> Cmd msg



-- AUDIO CONSTANTS


midiC4 : Int
midiC4 =
    60



-- Hardcoded C Major Scale: C4, D4, E4, F4, G4, A4, B4, C5


pitchRowToMidiNote : Int -> Int
pitchRowToMidiNote pitchRowIndex =
    case pitchRowIndex of
        0 ->
            midiC4

        -- C4 (60)
        1 ->
            midiC4 + 2

        -- D4 (62)
        2 ->
            midiC4 + 4

        -- E4 (64)
        3 ->
            midiC4 + 5

        -- F4 (65)
        4 ->
            midiC4 + 7

        -- G4 (67)
        5 ->
            midiC4 + 9

        -- A4 (69)
        6 ->
            midiC4 + 11

        -- B4 (71)
        7 ->
            midiC4 + 12

        -- C5 (72)
        _ ->
            midiC4



-- Default to C4


percussionTypeToMidiNote : PercussionType -> Int
percussionTypeToMidiNote percussionType =
    case percussionType of
        Kick ->
            36

        -- Standard kick percussion MIDI note
        Snare ->
            38



-- Standard snare percussion MIDI note


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


type alias Position =
    { pitchRowIndex : Int, stepColumnIndex : Int }


type alias Grid =
    Set ( Int, Int )


type PercussionType
    = Kick
    | Snare


type alias PercussionPosition =
    { percussionType : PercussionType, stepColumnIndex : Int }


type alias PercussionGrid =
    Set ( Int, Int )


type DrawState
    = NotDrawing
    | DrawingPitch
    | ErasingPitch
    | DrawingPercussion
    | ErasingPercussion


type alias Model =
    { totalPitchRows : Int
    , totalStepColumns : Int
    , grid : Grid
    , percussionGrid : PercussionGrid
    , drawState : DrawState
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { totalPitchRows = 8
      , totalStepColumns = 16
      , grid = Set.empty
      , percussionGrid = Set.empty
      , drawState = NotDrawing
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartDrawing Position
    | ContinueDrawing Position
    | StopDrawing
    | StartDrawingPercussion PercussionPosition
    | ContinueDrawingPercussion PercussionPosition
    | StopDrawingPercussion
    | PlayPitchNote Int
    | PlayPercussionNote PercussionType


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawing position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isCellActive position model.grid

                        newDrawState =
                            if currentlyActive then
                                ErasingPitch

                            else
                                DrawingPitch

                        newGrid =
                            setCellActive position (not currentlyActive) model.grid

                        -- Play note when drawing (activating)
                        playCmd =
                            if not currentlyActive then
                                let
                                    midiNote =
                                        pitchRowToMidiNote position.pitchRowIndex
                                in
                                playNote { note = midiNote, duration = 0.5, volume = 0.7 }

                            else
                                Cmd.none
                    in
                    ( { model | drawState = newDrawState, grid = newGrid }, playCmd )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawing position ->
            case model.drawState of
                DrawingPitch ->
                    let
                        newGrid =
                            setCellActive position True model.grid

                        -- Play note for every cell during drawing
                        playCmd =
                            let
                                midiNote =
                                    pitchRowToMidiNote position.pitchRowIndex
                            in
                            playNote { note = midiNote, duration = 0.5, volume = 0.7 }
                    in
                    ( { model | grid = newGrid }, playCmd )

                ErasingPitch ->
                    let
                        newGrid =
                            setCellActive position False model.grid
                    in
                    ( { model | grid = newGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        StartDrawingPercussion position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isPercussionCellActive position model.percussionGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPercussion

                            else
                                DrawingPercussion

                        newPercussionGrid =
                            setPercussionCellActive position (not currentlyActive) model.percussionGrid

                        -- Play percussion note when drawing (activating)
                        playCmd =
                            if not currentlyActive then
                                let
                                    midiNote =
                                        percussionTypeToMidiNote position.percussionType
                                in
                                playPercussion { note = midiNote, duration = 0.5, volume = 0.8 }

                            else
                                Cmd.none
                    in
                    ( { model | drawState = newDrawState, percussionGrid = newPercussionGrid }, playCmd )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPercussion position ->
            case model.drawState of
                DrawingPercussion ->
                    let
                        newPercussionGrid =
                            setPercussionCellActive position True model.percussionGrid

                        -- Play percussion note for every cell during drawing
                        playCmd =
                            let
                                midiNote =
                                    percussionTypeToMidiNote position.percussionType
                            in
                            playPercussion { note = midiNote, duration = 0.5, volume = 0.8 }
                    in
                    ( { model | percussionGrid = newPercussionGrid }, playCmd )

                ErasingPercussion ->
                    let
                        newPercussionGrid =
                            setPercussionCellActive position False model.percussionGrid
                    in
                    ( { model | percussionGrid = newPercussionGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawingPercussion ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        PlayPitchNote pitchRowIndex ->
            let
                midiNote =
                    pitchRowToMidiNote pitchRowIndex

                cmd =
                    playNote { note = midiNote, duration = 0.5, volume = 0.7 }
            in
            ( model, cmd )

        PlayPercussionNote percussionType ->
            let
                midiNote =
                    percussionTypeToMidiNote percussionType

                cmd =
                    playPercussion { note = midiNote, duration = 0.5, volume = 0.8 }
            in
            ( model, cmd )



-- View


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-gray-900 text-white flex flex-col select-none" ]
        [ headerView
        , centerView model
        , footerView
        ]


headerView : Html Msg
headerView =
    div [ class "bg-gray-800 border-b border-gray-700 px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ div [ class "text-2xl font-bold text-white" ]
                [ text "Song Maker V2" ]
            , div [ class "flex items-center gap-4" ]
                [ div [ class "text-gray-300 text-sm" ]
                    [ text "BPM: 120" ]
                , div [ class "bg-fuchsia-700 hover:bg-fuchsia-800 text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer" ]
                    [ text "Play" ]
                ]
            ]
        ]


centerView : Model -> Html Msg
centerView model =
    div [ class "flex-1 overflow-auto" ]
        [ viewGrid model ]


viewGrid : { a | totalPitchRows : Int, totalStepColumns : Int, grid : Grid, percussionGrid : PercussionGrid } -> Html Msg
viewGrid { totalPitchRows, totalStepColumns, grid, percussionGrid } =
    let
        gridTemplateColumns =
            "minmax($labelColumnMinWidth, auto) repeat($stepColumnsCount, minmax($stepColumnMinWidth, 1fr))"
                |> String.replace "$labelColumnMinWidth" (px 48)
                |> String.replace "$stepColumnsCount" (String.fromInt totalStepColumns)
                |> String.replace "$stepColumnMinWidth" (px 48)

        gridTemplateRows =
            "minmax($headerRowMinHeight, auto) repeat($pitchRowsCount, minmax($rowMinHeight, 1fr)) repeat(2, $percussionRowHeight)"
                |> String.replace "$headerRowMinHeight" (px 32)
                |> String.replace "$pitchRowsCount" (String.fromInt totalPitchRows)
                |> String.replace "$rowMinHeight" (px 32)
                |> String.replace "$percussionRowHeight" (px 48)
    in
    div
        [ class "grid bg-gray-800 border border-gray-700 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateColumns
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class labelClass, class "border-b border-gray-600" ] [] ]
            ++ {- Step headers row -} times viewStepHeader totalStepColumns
            ++ {- Pitch rows -} (times (viewPitchRow totalStepColumns grid) totalPitchRows |> List.concat)
            ++ {- Percussion rows -} viewPercussionRows totalStepColumns percussionGrid
        )


viewStepHeader : Int -> Html Msg
viewStepHeader stepColumnIndex =
    div
        [ class labelClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepColumnIndex + 1)) ]


viewPitchRow : Int -> Grid -> Int -> List (Html Msg)
viewPitchRow stepCount grid pitchRowIndex =
    let
        viewPitchLabel =
            div
                [ class labelClass ]
                [ text ("Pitch " ++ String.fromInt (pitchRowIndex + 1)) ]
    in
    viewPitchLabel :: times (viewPitchCell pitchRowIndex grid) stepCount


viewPitchCell : Int -> Grid -> Int -> Html Msg
viewPitchCell pitchRowIndex grid stepColumnIndex =
    let
        position =
            { pitchRowIndex = pitchRowIndex, stepColumnIndex = stepColumnIndex }

        isActive =
            isCellActive position grid

        bgClass =
            if isActive then
                "bg-fuchsia-600 hover:bg-fuchsia-700"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class bgClass
        , class "border-r border-b border-gray-600 cursor-pointer transition-colors"
        , HE.onMouseDown (StartDrawing position)
        , HE.onMouseEnter (ContinueDrawing position)
        , HE.onMouseUp StopDrawing
        ]
        []


viewPercussionRows : Int -> PercussionGrid -> List (Html Msg)
viewPercussionRows stepCount percussionGrid =
    [ -- Snare percussion label (top row)
      div
        [ class labelClass ]
        [ text "Snare" ]
    ]
        ++ times (viewPercussionCell Snare percussionGrid) stepCount
        ++ [ -- Kick percussion label (bottom row)
             div
                [ class labelClass ]
                [ text "Kick" ]
           ]
        ++ times (viewPercussionCell Kick percussionGrid) stepCount


viewPercussionCell : PercussionType -> PercussionGrid -> Int -> Html Msg
viewPercussionCell percussionType percussionGrid stepColumnIndex =
    let
        position =
            { percussionType = percussionType, stepColumnIndex = stepColumnIndex }

        isActive =
            isPercussionCellActive position percussionGrid

        symbol =
            if isActive then
                case percussionType of
                    Kick ->
                        -- Circle symbol
                        div
                            [ class "w-6 h-6 bg-fuchsia-600 rounded-full"
                            ]
                            []

                    Snare ->
                        -- Triangle symbol
                        div
                            [ class "w-6 h-6 bg-fuchsia-600"
                            , style "clip-path" "polygon(50% 0%, 0% 100%, 100% 100%)"
                            ]
                            []

            else
                -- Small dim dot for inactive
                div
                    [ class "w-1.5 h-1.5 bg-gray-500 rounded-full"
                    ]
                    []
    in
    div
        [ class "bg-gray-800 hover:bg-gray-700 border-r border-b border-gray-600 cursor-pointer transition-colors flex items-center justify-center"
        , HE.onMouseDown (StartDrawingPercussion position)
        , HE.onMouseEnter (ContinueDrawingPercussion position)
        , HE.onMouseUp StopDrawingPercussion
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



-- Basic Helpers


times fn i =
    List.range 0 (i - 1) |> List.map fn



-- Conversion Functions


percussionTypeToPercussionRowIndex : PercussionType -> Int
percussionTypeToPercussionRowIndex percussionType =
    case percussionType of
        Snare ->
            0

        Kick ->
            1


percussionPositionToTuple : PercussionPosition -> ( Int, Int )
percussionPositionToTuple { percussionType, stepColumnIndex } =
    ( percussionTypeToPercussionRowIndex percussionType, stepColumnIndex )



-- Cell State Management


isCellActive : Position -> Grid -> Bool
isCellActive { pitchRowIndex, stepColumnIndex } grid =
    Set.member ( pitchRowIndex, stepColumnIndex ) grid


setCellActive : Position -> Bool -> Grid -> Grid
setCellActive { pitchRowIndex, stepColumnIndex } isActive grid =
    if isActive then
        Set.insert ( pitchRowIndex, stepColumnIndex ) grid

    else
        Set.remove ( pitchRowIndex, stepColumnIndex ) grid


isPercussionCellActive : PercussionPosition -> PercussionGrid -> Bool
isPercussionCellActive position grid =
    Set.member (percussionPositionToTuple position) grid


setPercussionCellActive : PercussionPosition -> Bool -> PercussionGrid -> PercussionGrid
setPercussionCellActive position isActive grid =
    let
        tuple =
            percussionPositionToTuple position
    in
    if isActive then
        Set.insert tuple grid

    else
        Set.remove tuple grid



-- Style Constants


labelClass : String
labelClass =
    "bg-gray-900 border-r border-gray-600 flex items-center justify-center text-xs font-bold text-white"



-- View Helpers


px f =
    String.fromFloat f ++ "px"
