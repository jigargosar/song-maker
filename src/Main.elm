port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Json.Decode as JD
import Model exposing (DrawState(..), Flags, Model, NoteToPlay, PlayState(..), ViewModel)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Timing exposing (TimeConfig)
import Url exposing (Url)
import Utils exposing (..)


{-| <https://surikov.github.io/webaudiofont/>
-}



-- PORTS


port playNote : NoteToPlay -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model.init {} url key, Cmd.none )


type Msg
    = NoOp
    | StartDrawingPitch PitchPos
    | ContinueDrawingPitch PitchPos
    | StopDrawing
    | StartDrawingPerc PercPos
    | ContinueDrawingPerc PercPos
    | Play
    | Stop
    | TimeSync Float
    | ChangeScaleType ScaleType
    | ChangeRootNote RootNote
    | ChangeOctaveStart Int
    | ChangeOctaveCount Int
    | ChangeBPM Int
    | ChangeTonalInstrument TonalInstrument
    | ChangeDrumKit DrumKit
    | ChangeBars Int
    | ChangeBeatsPerBar Int
    | ChangeSubdivisions Int
    | Undo
    | Redo
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Save


subscriptions : Model -> Sub Msg
subscriptions _ =
    timeSync TimeSync


playMaybeNote : Maybe NoteToPlay -> Cmd msg
playMaybeNote maybeNote =
    case maybeNote of
        Just note ->
            playNote note

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawingPitch position ->
            let
                ( newModel, maybeNote ) =
                    Model.startDrawingPitch position model
            in
            ( newModel, playMaybeNote maybeNote )

        ContinueDrawingPitch position ->
            let
                ( newModel, maybeNote ) =
                    Model.continueDrawingPitch position model
            in
            ( newModel, playMaybeNote maybeNote )

        StopDrawing ->
            ( Model.stopDrawing model, Cmd.none )

        StartDrawingPerc position ->
            let
                ( newModel, maybeNote ) =
                    Model.startDrawingPerc position model
            in
            ( newModel, playMaybeNote maybeNote )

        ContinueDrawingPerc position ->
            let
                ( newModel, maybeNote ) =
                    Model.continueDrawingPerc position model
            in
            ( newModel, playMaybeNote maybeNote )

        Play ->
            ( Model.startPlaying model, Cmd.none )

        Stop ->
            ( Model.stop model, Cmd.none )

        TimeSync audioContextTime ->
            let
                ( newModel, notesToPlay ) =
                    Model.onTimeSync audioContextTime model

                playCommands =
                    List.map playNote notesToPlay |> Cmd.batch
            in
            ( newModel, playCommands )

        ChangeScaleType newScaleType ->
            ( Model.changeScaleType newScaleType model, Cmd.none )

        ChangeRootNote newRootNote ->
            ( Model.changeRootNote newRootNote model, Cmd.none )

        ChangeOctaveStart newStart ->
            ( Model.changeOctaveStart newStart model, Cmd.none )

        ChangeOctaveCount newCount ->
            ( Model.changeOctaveCount newCount model, Cmd.none )

        ChangeBPM newBPM ->
            ( Model.setBPM newBPM model, Cmd.none )

        ChangeTonalInstrument newInstrument ->
            ( Model.setTonalInstrument newInstrument model, Cmd.none )

        ChangeDrumKit newDrumKit ->
            ( Model.setDrumKit newDrumKit model, Cmd.none )

        ChangeBars bars ->
            ( Model.changeBars bars model, Cmd.none )

        ChangeBeatsPerBar newBeatsPerBar ->
            ( Model.changeBeatsPerBar newBeatsPerBar model, Cmd.none )

        ChangeSubdivisions subDivisions ->
            ( Model.changeSubdivisions subDivisions model, Cmd.none )

        Undo ->
            ( Model.undo model, Cmd.none )

        Redo ->
            ( Model.redo model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( Model.loadFromUrl url model, Cmd.none )

        Save ->
            let
                saveCommand =
                    case Model.getSaveAction model of
                        Just ( key, query ) ->
                            Nav.pushUrl key query

                        Nothing ->
                            Cmd.none
            in
            ( model, saveCommand )



-- View


view : Model -> Document Msg
view model =
    let
        vm =
            Model.toVm model
    in
    { title = "SM"
    , body =
        [ div [ class "h-screen bg-gray-900 text-white flex flex-col select-none" ]
            [ viewHeader model
            , centerView vm model
            , footerView vm model
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bg-gray-800 z-20 shadow-2xl border-b border-gray-950 px-6 py-4" ]
        [ div [ class "flex flex-wrap items-center justify-between" ]
            [ div [ class "flex flex-wrap items-center gap-6" ]
                [ div [ class "text-2xl font-bold text-white" ] [ text "Song Maker V2" ]
                , viewScaleControls model
                , viewSequenceControls model
                ]
            ]
        ]


centerView : ViewModel -> Model -> Html Msg
centerView vm model =
    div [ class "flex-1 overflow-auto" ] [ viewGrid vm model ]


viewGrid : ViewModel -> Model -> Html Msg
viewGrid vm model =
    let
        currentStep =
            Model.getCurrentPlayingStep model

        gridTemplateCols =
            format "minmax($pitchLabelColMinWidth, auto) repeat($totalSteps, minmax($stepColMinWidth, 1fr))"
                [ ( "$pitchLabelColMinWidth", px 48 )
                , ( "$totalSteps", String.fromInt vm.totalSteps )
                , ( "$stepColMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalPitches, minmax($pitchRowMinHeight, 1fr)) repeat(2, $percRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalPitches", String.fromInt vm.totalPitches )
                , ( "$pitchRowMinHeight", px 32 )
                , ( "$percRowHeight", px 48 )
                ]
    in
    div
        [ class "grid bg-gray-800 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateCols
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class labelBgColorAndClass, class "border-b border-gray-600" ] [] ]
            ++ {- Step Labels row -} times (\stepIdx -> viewStepLabel stepIdx (vm.isStepCurrentlyPlaying stepIdx)) vm.totalSteps
            ++ {- Pitch rows -} (times (viewPitchRow vm model currentStep) vm.totalPitches |> List.concat)
            ++ {- Perc Snare row -} viewPercRow Instruments.percSnare vm.totalSteps model.percGrid currentStep
            ++ {- Perc Kick row -} viewPercRow Instruments.percKick vm.totalSteps model.percGrid currentStep
        )


viewStepLabel : Int -> Bool -> Html Msg
viewStepLabel stepIdx isPlaying =
    let
        bgClass =
            if isPlaying then
                accentBgColor

            else
                labelBgColor
    in
    div
        [ class labelClass, class bgClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepIdx + 1)) ]


viewPitchRow : ViewModel -> Model -> Maybe Int -> Int -> List (Html Msg)
viewPitchRow vm model currentStep pitchIdx =
    let
        viewPitchLabel =
            div
                [ class labelBgColorAndClass, class "border-[0.5px]" ]
                [ text (Scales.pitchIdxToNoteName pitchIdx (Model.scaleConfig model)) ]
    in
    viewPitchLabel :: times (\stepIdx -> viewPitchCell vm pitchIdx currentStep stepIdx) vm.totalSteps


viewPitchCell : ViewModel -> Int -> Maybe Int -> Int -> Html Msg
viewPitchCell vm pitchIdx currentStep stepIdx =
    let
        position =
            { pitchIdx = pitchIdx, stepIdx = stepIdx }

        isActive =
            vm.isPitchCellActive position

        isCurrentStep =
            currentStep == Just stepIdx

        -- TODO: try refactor
        noteClass =
            if isActive then
                pitchCellColor pitchIdx

            else if isCurrentStep then
                "bg-gray-700 hover:bg-gray-600"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class noteClass
        , class "border-[0.5px] border-gray-600 cursor-pointer "
        , HE.custom "pointerdown"
            (JD.map2
                (\button isPrimary ->
                    if button == 0 && isPrimary then
                        { message = StartDrawingPitch position, stopPropagation = False, preventDefault = False }

                    else
                        { message = NoOp, stopPropagation = True, preventDefault = True }
                )
                (JD.field "button" JD.int)
                (JD.field "isPrimary" JD.bool)
            )
        , HE.on "pointerenter" (JD.succeed (ContinueDrawingPitch position))
        , HE.on "pointerup" (JD.succeed StopDrawing)
        ]
        []


viewPercRow : PercType -> Int -> PercGrid -> Maybe Int -> List (Html Msg)
viewPercRow percType totalSteps percGrid currentStep =
    let
        percTypeName =
            Instruments.percLabel percType

        stickyClass =
            case percType of
                _ ->
                    if percType == Instruments.percSnare then
                        "sticky bottom-12 h-12 z-10 border-t-3"

                    else
                        "sticky bottom-0 h-12 z-10"
    in
    div [ class labelBgColorAndClass, class stickyClass ] [ text percTypeName ]
        :: times (\stepIdx -> viewPercCell percType percGrid currentStep stepIdx) totalSteps


viewPercCell : PercType -> PercGrid -> Maybe Int -> Int -> Html Msg
viewPercCell percType percGrid currentStep stepIdx =
    let
        position =
            { percType = percType, stepIdx = stepIdx }

        isActive =
            Grid.isPercCellActive position percGrid

        isCurrentStep =
            currentStep == Just stepIdx

        symbol =
            viewPercSymbol isActive percType

        stickyClass =
            case percType of
                _ ->
                    if percType == Instruments.percSnare then
                        "sticky bottom-12 h-12 z-10  border-t-3"

                    else
                        "sticky bottom-0 h-12 z-10"

        cellClass =
            if isCurrentStep then
                -- TODO: do we need rings here?
                "bg-gray-700 hover:bg-gray-600"

            else
                "bg-gray-800 hover:bg-gray-700"
    in
    div
        [ class " border-gray-600 cursor-pointer  flex items-center justify-center"
        , class cellClass
        , class stickyClass
        , HE.custom "pointerdown"
            (JD.map2
                (\button isPrimary ->
                    if button == 0 && isPrimary then
                        { message = StartDrawingPerc position, stopPropagation = False, preventDefault = False }

                    else
                        { message = NoOp, stopPropagation = True, preventDefault = True }
                )
                (JD.field "button" JD.int)
                (JD.field "isPrimary" JD.bool)
            )
        , HE.on "pointerenter" (JD.succeed (ContinueDrawingPerc position))
        , HE.on "pointerup" (JD.succeed StopDrawing)
        ]
        [ symbol ]


footerView : ViewModel -> Model -> Html Msg
footerView vm model =
    div [ class "bg-gray-800 border-t border-gray-700 px-6 py-3" ]
        [ div [ class "flex items-center gap-6" ]
            [ viewPlayStopButton model.playState
            , viewTonalInstrumentSelector model.currentTonalInstrument
            , viewDrumKitSelector model.currentDrumKit
            , div [ class "flex items-center gap-2" ]
                [ H.label [ class "text-xs text-gray-400 font-medium" ] [ text "BPM" ]
                , viewBPMInput model.bpm
                ]
            , H.button
                [ class "bg-gray-700 text-white px-3 py-1 rounded disabled:opacity-50"
                , HE.onClick Undo
                , HA.disabled (not vm.canUndo)
                ]
                [ text "↶ Undo" ]
            , H.button
                [ class "bg-gray-700 text-white px-3 py-1 rounded disabled:opacity-50"
                , HE.onClick Redo
                , HA.disabled (not vm.canRedo)
                ]
                [ text "↷ Redo" ]
            , H.button
                [ class "bg-gray-700 text-white px-3 py-1 rounded hover:bg-gray-600 transition-colors"
                , HE.onClick Save
                ]
                [ text "Save" ]
            ]
        ]



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


labelBgColorAndClass : String
labelBgColorAndClass =
    labelClass ++ " " ++ labelBgColor


labelClass : String
labelClass =
    "border-r border-gray-600 flex items-center justify-center text-xs font-bold text-white"


labelBgColor : String
labelBgColor =
    "bg-gray-900"



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
            _ ->
                if percType == Instruments.percKick then
                    -- Circle symbol
                    div [ class "w-6 h-6 rounded-full", class accentBgColor ] []

                else
                    -- Triangle symbol
                    div [ class "w-6 h-6", class accentBgColor, style "clip-path" "polygon(50% 0%, 0% 100%, 100% 100%)" ] []

    else
        -- Small dim dot for inactive
        div [ class "w-1.5 h-1.5 bg-gray-500 rounded-full" ] []


{-| TODO: why mod 7 and how it works
it works for major and pentatonic, since we are doing mod by7
but doesnt work for chromatic
-}
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


viewScaleControls : Model -> Html Msg
viewScaleControls model =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Scale" (viewScaleTypeSelector model.scaleType)
        , viewControlGroup "Root" (viewRootNoteSelector model.rootNote)
        , viewControlGroup "Start" (viewOctaveStartInput model.octaveStart)
        , viewControlGroup "Count" (viewOctaveCountInput model.octaveCount)
        ]


viewSequenceControls : Model -> Html Msg
viewSequenceControls model =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Bars" (viewBarsInput model.bars)
        , viewControlGroup "Beats" (viewBeatsPerBarInput model.beatsPerBar)
        , viewControlGroup "Sub-div" (viewSubdivisionsInput model.subdivisions)
        ]


viewControlGroup : String -> Html Msg -> Html Msg
viewControlGroup labelText control =
    div [ class "flex flex-col items-center gap-1" ]
        [ H.label [ class "text-xs text-gray-400 font-medium" ]
            [ text labelText ]
        , control
        ]


viewScaleTypeSelector : ScaleType -> Html Msg
viewScaleTypeSelector currentScale =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Scales.parseScaleType >> ChangeScaleType)
        ]
        --[ H.option [ HA.value "Major", HA.selected (currentScale == Major) ] [ text "Major" ]
        --, H.option [ HA.value "Pentatonic", HA.selected (currentScale == Pentatonic) ] [ text "Pentatonic" ]
        --, H.option [ HA.value "Chromatic", HA.selected (currentScale == Chromatic) ] [ text "Chromatic" ]
        --]
        (Scales.allScales |> List.map (viewScaleOption currentScale))


viewScaleOption : ScaleType -> ScaleType -> Html msg
viewScaleOption currentScale scale =
    H.option [ HA.value (Scales.scaleLabel scale), HA.selected (currentScale == scale) ] [ text (Scales.scaleLabel scale) ]


viewRootNoteSelector : RootNote -> Html Msg
viewRootNoteSelector currentRoot =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Scales.parseRootNote >> ChangeRootNote)
        ]
        (List.map (viewRootNoteOption currentRoot) Scales.allRootNotes)


viewRootNoteOption : RootNote -> RootNote -> Html Msg
viewRootNoteOption currentRoot rootNote =
    H.option [ HA.value (Scales.rootNoteToString rootNote), HA.selected (currentRoot == rootNote) ]
        [ text (Scales.rootNoteToString rootNote) ]


viewOctaveStartInput : Int -> Html Msg
viewOctaveStartInput currentStart =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentStart)
        , HE.onInput (String.toInt >> Maybe.withDefault 4 >> ChangeOctaveStart)
        ]
        []


viewOctaveCountInput : Int -> Html Msg
viewOctaveCountInput currentCount =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentCount)
        , HE.onInput (String.toInt >> Maybe.withDefault 2 >> ChangeOctaveCount)
        ]
        []


viewBPMInput : Int -> Html Msg
viewBPMInput currentBPM =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentBPM)
        , HE.onInput (String.toInt >> Maybe.withDefault 120 >> ChangeBPM)
        ]
        []


viewBarsInput : Int -> Html Msg
viewBarsInput currentBars =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentBars)
        , HE.onInput (String.toInt >> Maybe.withDefault 4 >> ChangeBars)
        ]
        []


viewBeatsPerBarInput : Int -> Html Msg
viewBeatsPerBarInput currentBeatsPerBar =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentBeatsPerBar)
        , HE.onInput (String.toInt >> Maybe.withDefault 4 >> ChangeBeatsPerBar)
        ]
        []


viewSubdivisionsInput : Int -> Html Msg
viewSubdivisionsInput currentSubdivisions =
    H.input
        [ HA.type_ "number"
        , class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 w-16 text-center hover:bg-gray-600 transition-colors"
        , HA.value (String.fromInt currentSubdivisions)
        , HE.onInput (String.toInt >> Maybe.withDefault 1 >> ChangeSubdivisions)
        ]
        []


viewTonalInstrumentSelector : TonalInstrument -> Html Msg
viewTonalInstrumentSelector currentInstrument =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseTonal >> ChangeTonalInstrument)
        ]
        (List.map (viewTonalInstrumentOption currentInstrument) Instruments.allTonal)


viewDrumKitSelector : DrumKit -> Html Msg
viewDrumKitSelector currentDrumKit =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseDrumKit >> ChangeDrumKit)
        ]
        (List.map (viewDrumKitOption currentDrumKit) Instruments.allDrumKits)


viewTonalInstrumentOption : TonalInstrument -> TonalInstrument -> Html Msg
viewTonalInstrumentOption currentInstrument instrument =
    H.option
        [ HA.value (Instruments.tonalLabel instrument)
        , HA.selected (currentInstrument == instrument)
        ]
        [ text (Instruments.tonalLabel instrument) ]


viewDrumKitOption : DrumKit -> DrumKit -> Html Msg
viewDrumKitOption currentDrumKit drumKit =
    H.option
        [ HA.value (Instruments.drumKitLabel drumKit)
        , HA.selected (currentDrumKit == drumKit)
        ]
        [ text (Instruments.drumKitLabel drumKit) ]
