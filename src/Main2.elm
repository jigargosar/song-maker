port module Main2 exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Set exposing (Set)



-- PORTS


port playPitch : { note : Int, duration : Float, volume : Float } -> Cmd msg


port playPercussion : { note : Int, duration : Float, volume : Float } -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



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


type alias PitchGrid =
    Set ( Int, Int )


type PercussionType
    = Kick
    | Snare


type alias PercussionPosition =
    { percussionType : PercussionType, stepColumnIndex : Int }


type alias PercussionGrid =
    Set ( Int, Int )


type PlayState
    = Stopped
    | PlayingStarted { startTime : Float }
    | Playing { startTime : Float, nextStep : Int }


type DrawState
    = NotDrawing
    | DrawingPitch
    | ErasingPitch
    | DrawingPercussion
    | ErasingPercussion


type alias Model =
    { totalPitchRows : Int
    , totalStepColumns : Int
    , pitchGrid : PitchGrid
    , percussionGrid : PercussionGrid
    , drawState : DrawState
    , playState : PlayState
    , audioContextTime : Float
    , bpm : Int
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { totalPitchRows = 8
      , totalStepColumns = 16
      , pitchGrid = Set.empty
      , percussionGrid = Set.empty
      , drawState = NotDrawing
      , playState = Stopped
      , audioContextTime = 0.0
      , bpm = 120
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
    | Play
    | Stop
    | TimeSync Float


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
                            isPitchCellActive position model.pitchGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPitch

                            else
                                DrawingPitch
                    in
                    ( { model
                        | drawState = newDrawState
                        , pitchGrid = updatePitchCell position (not currentlyActive) model.pitchGrid
                      }
                    , playPitchCmdIf (not currentlyActive) position.pitchRowIndex
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawing position ->
            case model.drawState of
                DrawingPitch ->
                    ( { model | pitchGrid = updatePitchCell position True model.pitchGrid }
                    , playPitchCmdIf True position.pitchRowIndex
                    )

                ErasingPitch ->
                    ( { model | pitchGrid = updatePitchCell position False model.pitchGrid }, Cmd.none )

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
                    in
                    ( { model
                        | drawState = newDrawState
                        , percussionGrid = updatePercussionCell position (not currentlyActive) model.percussionGrid
                      }
                    , playPercussionCmdIf (not currentlyActive) position.percussionType
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPercussion position ->
            case model.drawState of
                DrawingPercussion ->
                    ( { model | percussionGrid = updatePercussionCell position True model.percussionGrid }
                    , playPercussionCmdIf True position.percussionType
                    )

                ErasingPercussion ->
                    ( { model | percussionGrid = updatePercussionCell position False model.percussionGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawingPercussion ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        PlayPitchNote pitchRowIndex ->
            ( model, playPitchCmdIf True pitchRowIndex )

        PlayPercussionNote percussionType ->
            ( model, playPercussionCmdIf True percussionType )

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
            ( { model | audioContextTime = audioContextTime }, Cmd.none )



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
                , div
                    [ class accentBgColorWithHover
                    , class "text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer"
                    ]
                    [ text "Play" ]
                ]
            ]
        ]


centerView : Model -> Html Msg
centerView model =
    div [ class "flex-1 overflow-auto" ]
        [ viewGrid model ]


format : String -> List ( String, String ) -> String
format templateString replacements =
    List.foldr (\( a, b ) -> String.replace a b) templateString replacements


viewGrid : { a | totalPitchRows : Int, totalStepColumns : Int, pitchGrid : PitchGrid, percussionGrid : PercussionGrid } -> Html Msg
viewGrid { totalPitchRows, totalStepColumns, pitchGrid, percussionGrid } =
    let
        gridTemplateColumns =
            format "minmax($pitchLabelColumnMinWidth, auto) repeat($totalStepColumns, minmax($stepColumnMinWidth, 1fr))"
                [ ( "$pitchLabelColumnMinWidth", px 48 )
                , ( "$totalStepColumns", String.fromInt totalStepColumns )
                , ( "$stepColumnMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalPitchRows, minmax($pitchRowMinHeight, 1fr)) repeat(2, $percussionRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalPitchRows", String.fromInt totalPitchRows )
                , ( "$pitchRowMinHeight", px 32 )
                , ( "$percussionRowHeight", px 48 )
                ]
    in
    div
        [ class "grid bg-gray-800 border border-gray-700 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateColumns
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class labelClass, class "border-b border-gray-600" ] [] ]
            ++ {- Step headers row -} times viewStepHeader totalStepColumns
            ++ {- Pitch rows -} (times (viewPitchRow totalStepColumns pitchGrid) totalPitchRows |> List.concat)
            ++ {- Percussion Snare row -} viewPercussionRow Snare totalStepColumns percussionGrid
            ++ {- Percussion Kick row -} viewPercussionRow Kick totalStepColumns percussionGrid
        )


viewStepHeader : Int -> Html Msg
viewStepHeader stepColumnIndex =
    div
        [ class labelClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepColumnIndex + 1)) ]


viewPitchRow : Int -> PitchGrid -> Int -> List (Html Msg)
viewPitchRow stepCount pitchGrid pitchRowIndex =
    let
        viewPitchLabel =
            div
                [ class labelClass ]
                [ text ("Pitch " ++ String.fromInt (pitchRowIndex + 1)) ]
    in
    viewPitchLabel :: times (viewPitchCell pitchRowIndex pitchGrid) stepCount


viewPitchCell : Int -> PitchGrid -> Int -> Html Msg
viewPitchCell pitchRowIndex pitchGrid stepColumnIndex =
    let
        position =
            { pitchRowIndex = pitchRowIndex, stepColumnIndex = stepColumnIndex }

        isActive =
            isPitchCellActive position pitchGrid

        noteClass =
            if isActive then
                pitchCellColor pitchRowIndex

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class noteClass
        , class "border-r border-b border-gray-600 cursor-pointer"
        , HE.onMouseDown (StartDrawing position)
        , HE.onMouseEnter (ContinueDrawing position)
        , HE.onMouseUp StopDrawing
        ]
        []


viewPercussionRow : PercussionType -> Int -> PercussionGrid -> List (Html Msg)
viewPercussionRow percussionType totalStepColumns percussionGrid =
    let
        percussionTypeName =
            case percussionType of
                Snare ->
                    "Snare"

                Kick ->
                    "Kick"
    in
    div [ class labelClass ] [ text percussionTypeName ]
        :: times (viewPercussionCell percussionType percussionGrid) totalStepColumns


viewPercussionCell : PercussionType -> PercussionGrid -> Int -> Html Msg
viewPercussionCell percussionType percussionGrid stepColumnIndex =
    let
        position =
            { percussionType = percussionType, stepColumnIndex = stepColumnIndex }

        isActive =
            isPercussionCellActive position percussionGrid

        symbol =
            viewPercussionSymbol isActive percussionType
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


toPercussionRowIndex : PercussionType -> Int
toPercussionRowIndex percussionType =
    case percussionType of
        Snare ->
            0

        Kick ->
            1


percussionPositionToTuple : PercussionPosition -> ( Int, Int )
percussionPositionToTuple { percussionType, stepColumnIndex } =
    ( toPercussionRowIndex percussionType, stepColumnIndex )



-- Sequencer Functions


noteDuration : Model -> Float
noteDuration model =
    (60.0 / toFloat model.bpm) / 4.0


getCurrentPlayingStep : Model -> Maybe Int
getCurrentPlayingStep model =
    case model.playState of
        PlayingStarted _ ->
            Just 0

        Playing { nextStep } ->
            Just (modBy model.totalStepColumns (nextStep - 1))

        Stopped ->
            Nothing


type alias NoteToPlay =
    { note : Int, duration : Float, volume : Float }


getActiveNotesForStep : Int -> Model -> List NoteToPlay
getActiveNotesForStep stepIndex model =
    let
        duration =
            noteDuration model

        pitchNotes =
            times
                (\pitchRowIndex ->
                    let
                        position =
                            { pitchRowIndex = pitchRowIndex, stepColumnIndex = stepIndex }
                    in
                    if isPitchCellActive position model.pitchGrid then
                        Just
                            { note = pitchRowToMidiNote pitchRowIndex
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                model.totalPitchRows
                |> List.filterMap identity

        percussionNotes =
            [ Kick, Snare ]
                |> List.filterMap
                    (\percussionType ->
                        let
                            position =
                                { percussionType = percussionType, stepColumnIndex = stepIndex }
                        in
                        if isPercussionCellActive position model.percussionGrid then
                            Just
                                { note = percussionTypeToMidiNote percussionType
                                , duration = duration
                                , volume = 0.8
                                }

                        else
                            Nothing
                    )
    in
    pitchNotes ++ percussionNotes


-- Audio Helper Functions


playPitchCmdIf : Bool -> Int -> Cmd Msg
playPitchCmdIf shouldPlay pitchRowIndex =
    if shouldPlay then
        playPitch { note = pitchRowToMidiNote pitchRowIndex, duration = 0.5, volume = 0.7 }

    else
        Cmd.none


playPercussionCmdIf : Bool -> PercussionType -> Cmd Msg
playPercussionCmdIf shouldPlay percussionType =
    if shouldPlay then
        playPercussion { note = percussionTypeToMidiNote percussionType, duration = 0.5, volume = 0.8 }

    else
        Cmd.none



-- Cell State Management


isPitchCellActive : Position -> PitchGrid -> Bool
isPitchCellActive { pitchRowIndex, stepColumnIndex } pitchGrid =
    Set.member ( pitchRowIndex, stepColumnIndex ) pitchGrid


updatePitchCell : Position -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchRowIndex, stepColumnIndex } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchRowIndex, stepColumnIndex ) pitchGrid

    else
        Set.remove ( pitchRowIndex, stepColumnIndex ) pitchGrid


isPercussionCellActive : PercussionPosition -> PercussionGrid -> Bool
isPercussionCellActive position grid =
    Set.member (percussionPositionToTuple position) grid


updatePercussionCell : PercussionPosition -> Bool -> PercussionGrid -> PercussionGrid
updatePercussionCell position isActive grid =
    let
        tuple =
            percussionPositionToTuple position
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


labelClass : String
labelClass =
    "bg-gray-900 border-r border-gray-600 flex items-center justify-center text-xs font-bold text-white"



-- View Helpers


viewPercussionSymbol : Bool -> PercussionType -> Html Msg
viewPercussionSymbol isActive percussionType =
    if isActive then
        case percussionType of
            Kick ->
                -- Circle symbol
                div [ class "w-6 h-6 rounded-full", class accentBgColor ] []

            Snare ->
                -- Triangle symbol
                div [ class "w-6 h-6", class accentBgColor, style "clip-path" "polygon(50% 0%, 0% 100%, 100% 100%)" ] []

    else
        -- Small dim dot for inactive
        div [ class "w-1.5 h-1.5 bg-gray-500 rounded-full" ] []


pitchCellColor : Int -> String
pitchCellColor pitchRowIndex =
    case modBy 7 pitchRowIndex of
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



-- Fallback


px f =
    String.fromFloat f ++ "px"


{-
SEQUENCER IMPLEMENTATION PLAN
============================

Based on V1 architecture analysis, the sequencer needs these components:

1. DATA TYPES & MODEL UPDATES:
   - Add PlayState type: Stopped | PlayingStarted { startTime : Float } | Playing { startTime : Float, nextStep : Int }
   - Add model fields: playState : PlayState, audioContextTime : Float, bpm : Int (default 120)
   - The nextStep represents the step TO BE SCHEDULED (not already scheduled)

2. PORTS & MESSAGING:
   - Add timeSync port: timeSync : (Float -> msg) -> Sub msg
   - Add messages: Play | Stop | TimeSync Float
   - JavaScript will send audioContext.currentTime via timeSync using requestAnimationFrame

3. TIMING CALCULATIONS:
   - totalSteps: totalStepColumns (16 in our case)
   - noteDuration: (60.0 / bpm) / 4.0 (assuming 16th notes at 120 BPM = 0.125 seconds)
   - stepTime: startTime + (nextStep * noteDuration)

4. STATE TRANSITIONS:
   - Stopped → PlayingStarted: On Play button press, record startTime = audioContextTime
   - PlayingStarted → Playing: When first step gets scheduled (nextStep = 1)
   - Playing → Stopped: On Stop button press
   - Playing loops: Use modBy totalSteps for grid position

5. SCHEDULING LOGIC (TimeSync handler):
   - Calculate elapsedTime = audioContextTime - startTime
   - Calculate absoluteStep = floor(elapsedTime / noteDuration)
   - If absoluteStep >= nextStep, schedule notes for current step
   - Get active notes using getActiveNotesForStep(modBy totalSteps nextStep)
   - Play chord using existing playPitch/playPercussion ports
   - Increment nextStep after scheduling

6. VISUAL FEEDBACK:
   - getCurrentPlayingStep: PlayingStarted → Just 0, Playing → Just (modBy totalSteps (nextStep - 1))
   - Highlight current step in grid view
   - Update Play button to show Stop when playing

TODO LIST:
1. Add PlayState type (Stopped, PlayingStarted, Playing)
2. Add sequencer fields to Model (playState, audioContextTime, bpm)
3. Add sequencer messages (Play, Stop, TimeSync)
4. Add timeSync port for audio context time updates
6. Implement Play message handler (Stopped → PlayingStarted)
7. Implement Stop message handler (Playing/PlayingStarted → Stopped)
8. Implement TimeSync message handler with scheduling logic
9. Add totalSteps helper function for step count calculation
10. Add noteDuration helper function for BPM timing
11. Add getCurrentPlayingStep function for step highlighting
12. Add getActiveNotesForStep function for chord collection
13. Update Play button in headerView to be functional
14. Add timeSync subscription to subscriptions function
15. Update JavaScript to implement timeSync port with requestAnimationFrame
16. Test basic play/stop functionality
17. Test step progression and looping behavior
18. Test audio scheduling accuracy and timing
-}
