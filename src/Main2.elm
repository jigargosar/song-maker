port module Main2 exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events as HE
import Set exposing (Set)



-- PORTS


port playNote : { instrument : String, midi : Int, duration : Float, volume : Float } -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- AUDIO CONSTANTS


midiC4 : Int
midiC4 =
    60



-- Hardcoded C Major Scale: C4, D4, E4, F4, G4, A4, B4, C5


pitchIdxToMidi : Int -> Int
pitchIdxToMidi idx =
    case idx of
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


instrumentName : Instrument -> String
instrumentName instrument =
    case instrument of
        IPiano ->
            "_tone_0250_SoundBlasterOld_sf2"

        IKick ->
            "_drum_36_0_SBLive_sf2"

        ISnare ->
            "_drum_38_0_SBLive_sf2"


percTypeToInstrument : PercType -> Instrument
percTypeToInstrument percType =
    case percType of
        Kick ->
            IKick

        Snare ->
            ISnare


percTypeToMidi : PercType -> Int
percTypeToMidi percType =
    case percType of
        Kick ->
            36

        -- Standard kick perc MIDI note
        Snare ->
            38



-- Standard snare perc MIDI note


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


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type Instrument
    = IPiano
    | IKick
    | ISnare


type PercType
    = Kick
    | Snare


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )


type PlayState
    = Stopped
    | PlayingStarted { startTime : Float }
    | Playing { startTime : Float, nextStep : Int }


type DrawState
    = NotDrawing
    | DrawingPitch
    | ErasingPitch
    | DrawingPerc
    | ErasingPerc


type alias Model =
    { totalPitches : Int
    , totalSteps : Int
    , pitchGrid : PitchGrid
    , percGrid : PercGrid
    , drawState : DrawState
    , playState : PlayState
    , audioContextTime : Float
    , bpm : Int
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { totalPitches = 8
      , totalSteps = 16
      , pitchGrid = Set.empty
      , percGrid = Set.empty
      , drawState = NotDrawing
      , playState = Stopped
      , audioContextTime = 0.0
      , bpm = 80
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartDrawingPitch PitchPos
    | ContinueDrawingPitch PitchPos
    | StopDrawing
    | StartDrawingPerc PercPos
    | ContinueDrawingPerc PercPos
    | PlayPitchNote Int
    | PlayPercNote PercType
    | Play
    | Stop
    | TimeSync Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    timeSync TimeSync


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawingPitch position ->
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
                    , playPitchCmdIf (not currentlyActive) position.pitchIdx
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPitch position ->
            case model.drawState of
                DrawingPitch ->
                    ( { model | pitchGrid = updatePitchCell position True model.pitchGrid }
                    , playPitchCmdIf True position.pitchIdx
                    )

                ErasingPitch ->
                    ( { model | pitchGrid = updatePitchCell position False model.pitchGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        StartDrawingPerc position ->
            case model.drawState of
                NotDrawing ->
                    let
                        currentlyActive =
                            isPercCellActive position model.percGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPerc

                            else
                                DrawingPerc
                    in
                    ( { model
                        | drawState = newDrawState
                        , percGrid = updatePercCell position (not currentlyActive) model.percGrid
                      }
                    , playPercCmdIf (not currentlyActive) position.percType
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPerc position ->
            case model.drawState of
                DrawingPerc ->
                    ( { model | percGrid = updatePercCell position True model.percGrid }
                    , playPercCmdIf True position.percType
                    )

                ErasingPerc ->
                    ( { model | percGrid = updatePercCell position False model.percGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayPitchNote pitchIdx ->
            ( model, playPitchCmdIf True pitchIdx )

        PlayPercNote percType ->
            ( model, playPercCmdIf True percType )

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
            let
                updatedModel =
                    { model | audioContextTime = audioContextTime }
            in
            case model.playState of
                PlayingStarted { startTime } ->
                    let
                        stepToSchedule =
                            0

                        activeNotes =
                            getActiveNotesForStep stepToSchedule updatedModel

                        playCommands =
                            List.map playNote activeNotes |> Cmd.batch

                        newPlayState =
                            Playing { startTime = startTime, nextStep = 1 }
                    in
                    ( { updatedModel | playState = newPlayState }, playCommands )

                Playing { startTime, nextStep } ->
                    let
                        elapsedTime =
                            audioContextTime - startTime

                        duration =
                            noteDuration model

                        currentStep =
                            floor (elapsedTime / duration)
                    in
                    if currentStep >= nextStep then
                        let
                            stepToSchedule =
                                modBy model.totalSteps nextStep

                            activeNotes =
                                getActiveNotesForStep stepToSchedule updatedModel

                            playCommands =
                                List.map playNote activeNotes |> Cmd.batch

                            newPlayState =
                                Playing { startTime = startTime, nextStep = nextStep + 1 }
                        in
                        ( { updatedModel | playState = newPlayState }, playCommands )

                    else
                        ( updatedModel, Cmd.none )

                Stopped ->
                    ( updatedModel, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-gray-900 text-white flex flex-col select-none" ]
        [ headerView model
        , centerView model
        , footerView
        ]


headerView : Model -> Html Msg
headerView model =
    div [ class "bg-gray-800 border-b border-gray-700 px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ div [ class "text-2xl font-bold text-white" ]
                [ text "Song Maker V2" ]
            , div [ class "flex items-center gap-4" ]
                [ div [ class "text-gray-300 text-sm" ]
                    [ text "BPM: 80" ]
                , viewPlayStopButton model.playState
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


viewGrid : Model -> Html Msg
viewGrid model =
    let
        currentStep =
            getCurrentPlayingStep model

        gridTemplateCols =
            format "minmax($pitchLabelColMinWidth, auto) repeat($totalSteps, minmax($stepColMinWidth, 1fr))"
                [ ( "$pitchLabelColMinWidth", px 48 )
                , ( "$totalSteps", String.fromInt model.totalSteps )
                , ( "$stepColMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalPitches, minmax($pitchRowMinHeight, 1fr)) repeat(2, $percRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalPitches", String.fromInt model.totalPitches )
                , ( "$pitchRowMinHeight", px 32 )
                , ( "$percRowHeight", px 48 )
                ]
    in
    div
        [ class "grid bg-gray-800 border border-gray-700 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateCols
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class labelClass, class "border-b border-gray-600" ] [] ]
            ++ {- Step headers row -} times (\stepIdx -> viewStepHeader currentStep stepIdx) model.totalSteps
            ++ {- Pitch rows -} (times (\pitchIdx -> viewPitchRow model.totalSteps model.pitchGrid currentStep pitchIdx) model.totalPitches |> List.concat)
            ++ {- Perc Snare row -} viewPercRow Snare model.totalSteps model.percGrid currentStep
            ++ {- Perc Kick row -} viewPercRow Kick model.totalSteps model.percGrid currentStep
        )


viewStepHeader : Maybe Int -> Int -> Html Msg
viewStepHeader currentStep stepIdx =
    let
        isCurrentStep =
            case currentStep of
                Just current ->
                    current == stepIdx

                Nothing ->
                    False

        headerClass =
            if isCurrentStep then
                labelClass ++ " " ++ accentBgColor

            else
                labelClass
    in
    div
        [ class headerClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepIdx + 1)) ]


viewPitchRow : Int -> PitchGrid -> Maybe Int -> Int -> List (Html Msg)
viewPitchRow stepCount pitchGrid currentStep pitchIdx =
    let
        viewPitchLabel =
            div
                [ class labelClass ]
                [ text ("Pitch " ++ String.fromInt (pitchIdx + 1)) ]
    in
    viewPitchLabel :: times (\stepIdx -> viewPitchCell pitchIdx pitchGrid currentStep stepIdx) stepCount


viewPitchCell : Int -> PitchGrid -> Maybe Int -> Int -> Html Msg
viewPitchCell pitchIdx pitchGrid currentStep stepIdx =
    let
        position =
            { pitchIdx = pitchIdx, stepIdx = stepIdx }

        isActive =
            isPitchCellActive position pitchGrid

        isCurrentStep =
            case currentStep of
                Just current ->
                    current == stepIdx

                Nothing ->
                    False

        noteClass =
            if isActive then
                pitchCellColor pitchIdx
                    ++ (if isCurrentStep then
                            " ring-2 ring-white"

                        else
                            ""
                       )

            else if isCurrentStep then
                "bg-gray-700 hover:bg-gray-600 ring-2 ring-white"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class noteClass
        , class "border-r border-b border-gray-600 cursor-pointer"
        , HE.onMouseDown (StartDrawingPitch position)
        , HE.onMouseEnter (ContinueDrawingPitch position)
        , HE.onMouseUp StopDrawing
        ]
        []


viewPercRow : PercType -> Int -> PercGrid -> Maybe Int -> List (Html Msg)
viewPercRow percType totalSteps percGrid currentStep =
    let
        percTypeName =
            case percType of
                Snare ->
                    "Snare"

                Kick ->
                    "Kick"
    in
    div [ class labelClass ] [ text percTypeName ]
        :: times (\stepIdx -> viewPercCell percType percGrid currentStep stepIdx) totalSteps


viewPercCell : PercType -> PercGrid -> Maybe Int -> Int -> Html Msg
viewPercCell percType percGrid currentStep stepIdx =
    let
        position =
            { percType = percType, stepIdx = stepIdx }

        isActive =
            isPercCellActive position percGrid

        isCurrentStep =
            case currentStep of
                Just current ->
                    current == stepIdx

                Nothing ->
                    False

        symbol =
            viewPercSymbol isActive percType

        cellClass =
            if isCurrentStep then
                "bg-gray-700 hover:bg-gray-600 border-r border-b border-gray-600 cursor-pointer transition-colors flex items-center justify-center ring-2 ring-white"

            else
                "bg-gray-800 hover:bg-gray-700 border-r border-b border-gray-600 cursor-pointer transition-colors flex items-center justify-center"
    in
    div
        [ class cellClass
        , HE.onMouseDown (StartDrawingPerc position)
        , HE.onMouseEnter (ContinueDrawingPerc position)
        , HE.onMouseUp StopDrawing
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


toPercRowIdx : PercType -> Int
toPercRowIdx percType =
    case percType of
        Snare ->
            0

        Kick ->
            1


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( toPercRowIdx percType, stepIdx )



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
            Just (modBy model.totalSteps (nextStep - 1))

        Stopped ->
            Nothing


type alias NoteToPlay =
    { instrument : String, midi : Int, duration : Float, volume : Float }


getActiveNotesForStep : Int -> Model -> List NoteToPlay
getActiveNotesForStep stepIdx model =
    let
        duration =
            noteDuration model

        pitchNotes =
            times
                (\pitchIdx ->
                    let
                        position =
                            { pitchIdx = pitchIdx, stepIdx = stepIdx }
                    in
                    if isPitchCellActive position model.pitchGrid then
                        Just
                            { instrument = instrumentName IPiano
                            , midi = pitchIdxToMidi pitchIdx
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                model.totalPitches
                |> List.filterMap identity

        percNotes =
            [ Kick, Snare ]
                |> List.filterMap
                    (\percType ->
                        let
                            position =
                                { percType = percType, stepIdx = stepIdx }
                        in
                        if isPercCellActive position model.percGrid then
                            Just
                                { instrument = instrumentName (percTypeToInstrument percType)
                                , midi = percTypeToMidi percType
                                , duration = duration
                                , volume = 0.8
                                }

                        else
                            Nothing
                    )
    in
    pitchNotes ++ percNotes



-- Audio Helper Functions


playPitchCmdIf : Bool -> Int -> Cmd Msg
playPitchCmdIf shouldPlay pitchIdx =
    if shouldPlay then
        playNote
            { instrument = instrumentName IPiano
            , midi = pitchIdxToMidi pitchIdx
            , duration = 0.5
            , volume = 0.7
            }

    else
        Cmd.none


playPercCmdIf : Bool -> PercType -> Cmd Msg
playPercCmdIf shouldPlay percType =
    if shouldPlay then
        playNote
            { instrument = instrumentName (percTypeToInstrument percType)
            , midi = percTypeToMidi percType
            , duration = 0.5
            , volume = 0.8
            }

    else
        Cmd.none



-- Cell State Management


isPitchCellActive : PitchPos -> PitchGrid -> Bool
isPitchCellActive { pitchIdx, stepIdx } pitchGrid =
    Set.member ( pitchIdx, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchIdx, stepIdx ) pitchGrid

    else
        Set.remove ( pitchIdx, stepIdx ) pitchGrid


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    Set.member (percPositionToTuple position) grid


updatePercCell : PercPos -> Bool -> PercGrid -> PercGrid
updatePercCell position isActive grid =
    let
        tuple =
            percPositionToTuple position
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


viewPlayStopButton : PlayState -> Html Msg
viewPlayStopButton playState =
    let
        ( buttonText, buttonMsg ) =
            case playState of
                Stopped ->
                    ( "Play", Play )

                _ ->
                    ( "Stop", Stop )
    in
    div
        [ class accentBgColorWithHover
        , class "text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer"
        , HE.onClick buttonMsg
        ]
        [ text buttonText ]


viewPercSymbol : Bool -> PercType -> Html Msg
viewPercSymbol isActive percType =
    if isActive then
        case percType of
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
pitchCellColor pitchIdx =
    case modBy 7 pitchIdx of
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
