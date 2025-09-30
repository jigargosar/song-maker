port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Json.Decode as JD
import Model exposing (DrawState, Flags, Model, NoteToPlay, PlayState, ViewModel)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
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
            [ viewHeader vm model
            , centerView vm model
            , footerView vm model
            ]
        ]
    }


viewHeader : ViewModel -> Model -> Html Msg
viewHeader vm model =
    div [ class "bg-gray-800 z-20 shadow-2xl border-b border-gray-950 px-6 py-4" ]
        [ div [ class "flex flex-wrap items-center justify-between" ]
            [ div [ class "flex flex-wrap items-center gap-6" ]
                [ div [ class "text-2xl font-bold text-white" ] [ text "Song Maker V2" ]
                , viewScaleControls vm model
                , viewSequenceControls vm
                ]
            ]
        ]


centerView : ViewModel -> Model -> Html Msg
centerView vm model =
    div [ class "flex-1 overflow-auto" ] [ viewGrid vm model ]


viewGrid : ViewModel -> Model -> Html Msg
viewGrid vm model =
    let
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
            ++ {- Pitch rows -} (times (viewPitchRow vm model) vm.totalPitches |> List.concat)
            ++ {- Perc Snare row -} viewPercRow vm Instruments.percSnare
            ++ {- Perc Kick row -} viewPercRow vm Instruments.percKick
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


viewPitchRow : ViewModel -> Model -> Int -> List (Html Msg)
viewPitchRow vm model pitchIdx =
    let
        viewPitchLabel =
            div
                [ class labelBgColorAndClass, class "border-[0.5px]" ]
                [ text (vm.pitchIdxToNoteName pitchIdx) ]
    in
    viewPitchLabel :: times (\stepIdx -> viewPitchCell vm pitchIdx stepIdx) vm.totalSteps


viewPitchCell : ViewModel -> Int -> Int -> Html Msg
viewPitchCell vm pitchIdx stepIdx =
    let
        position =
            { pitchIdx = pitchIdx, stepIdx = stepIdx }

        isActive =
            vm.isPitchCellActive position

        isCurrentStep =
            vm.isStepCurrentlyPlaying stepIdx

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


viewPercRow : ViewModel -> PercType -> List (Html Msg)
viewPercRow vm percType =
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
        :: times (\stepIdx -> viewPercCell vm percType stepIdx) vm.totalSteps


viewPercCell : ViewModel -> PercType -> Int -> Html Msg
viewPercCell vm percType stepIdx =
    let
        position =
            { percType = percType, stepIdx = stepIdx }

        isActive =
            vm.isPercCellActive position

        isCurrentStep =
            vm.isStepCurrentlyPlaying stepIdx

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
            [ viewPlayStopButton vm
            , viewTonalInstrumentSelector vm
            , viewDrumKitSelector vm
            , div [ class "flex items-center gap-2" ]
                [ H.label [ class "text-xs text-gray-400 font-medium" ] [ text "BPM" ]
                , viewBPMInput vm.bpm
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


viewPlayStopButton : ViewModel -> Html Msg
viewPlayStopButton vm =
    let
        ( buttonText, buttonMsg ) =
            if vm.isPlaying then
                ( "Stop", Stop )
            else
                ( "Play", Play )
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


viewScaleControls : ViewModel -> Model -> Html Msg
viewScaleControls vm model =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Scale" (viewScaleTypeSelector vm)
        , viewControlGroup "Root" (viewRootNoteSelector vm)
        , viewControlGroup "Start" (viewOctaveStartInput vm.octaveStart)
        , viewControlGroup "Count" (viewOctaveCountInput vm.octaveCount)
        ]


viewSequenceControls : ViewModel -> Html Msg
viewSequenceControls vm =
    div [ class "flex items-center gap-4" ]
        [ viewControlGroup "Bars" (viewBarsInput vm.bars)
        , viewControlGroup "Beats" (viewBeatsPerBarInput vm.beatsPerBar)
        , viewControlGroup "Sub-div" (viewSubdivisionsInput vm.subdivisions)
        ]


viewControlGroup : String -> Html Msg -> Html Msg
viewControlGroup labelText control =
    div [ class "flex flex-col items-center gap-1" ]
        [ H.label [ class "text-xs text-gray-400 font-medium" ]
            [ text labelText ]
        , control
        ]


viewScaleTypeSelector : ViewModel -> Html Msg
viewScaleTypeSelector vm =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Scales.parseScaleType >> ChangeScaleType)
        ]
        --[ H.option [ HA.value "Major", HA.selected (currentScale == Major) ] [ text "Major" ]
        --, H.option [ HA.value "Pentatonic", HA.selected (currentScale == Pentatonic) ] [ text "Pentatonic" ]
        --, H.option [ HA.value "Chromatic", HA.selected (currentScale == Chromatic) ] [ text "Chromatic" ]
        --]
        (Scales.allScales |> List.map (viewScaleOption vm))


viewScaleOption : ViewModel -> ScaleType -> Html msg
viewScaleOption vm scale =
    H.option [ HA.value (Scales.scaleLabel scale), HA.selected (vm.isScaleSelected scale) ] [ text (Scales.scaleLabel scale) ]


viewRootNoteSelector : ViewModel -> Html Msg
viewRootNoteSelector vm =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Scales.parseRootNote >> ChangeRootNote)
        ]
        (List.map (viewRootNoteOption vm) Scales.allRootNotes)


viewRootNoteOption : ViewModel -> RootNote -> Html Msg
viewRootNoteOption vm rootNote =
    H.option [ HA.value (Scales.rootNoteToString rootNote), HA.selected (vm.isRootNoteSelected rootNote) ]
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


viewTonalInstrumentSelector : ViewModel -> Html Msg
viewTonalInstrumentSelector vm =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseTonal >> ChangeTonalInstrument)
        ]
        (List.map (viewTonalInstrumentOption vm) Instruments.allTonal)


viewDrumKitSelector : ViewModel -> Html Msg
viewDrumKitSelector vm =
    H.select
        [ class "bg-gray-700 text-white text-sm border border-gray-600 rounded px-2 py-1 cursor-pointer hover:bg-gray-600 transition-colors"
        , HE.onInput (Instruments.parseDrumKit >> ChangeDrumKit)
        ]
        (List.map (viewDrumKitOption vm) Instruments.allDrumKits)


viewTonalInstrumentOption : ViewModel -> TonalInstrument -> Html Msg
viewTonalInstrumentOption vm instrument =
    H.option
        [ HA.value (Instruments.tonalLabel instrument)
        , HA.selected (vm.isTonalInstrumentSelected instrument)
        ]
        [ text (Instruments.tonalLabel instrument) ]


viewDrumKitOption : ViewModel -> DrumKit -> Html Msg
viewDrumKitOption vm drumKit =
    H.option
        [ HA.value (Instruments.drumKitLabel drumKit)
        , HA.selected (vm.isDrumKitSelected drumKit)
        ]
        [ text (Instruments.drumKitLabel drumKit) ]



{-
   ## ViewModel Migration Strategy & Current State

   ### Goal & Why We're Pursuing It
   **Goal**: Eliminate all direct model access from view functions to create a clean separation between model structure and view requirements.

   **Why**:
   - **Encapsulation**: Views shouldn't depend on internal model structure
   - **Performance**: Pre-compute expensive operations once instead of repeatedly
   - **View-optimized data**: Serve data in the format views actually need (e.g., `canUndo : Bool` instead of `List.isEmpty model.undoStack`)
   - **Future flexibility**: Can change model structure without breaking views
   - **Cleaner code**: Views focus on presentation, not data manipulation

   ### What We've Learned
   1. **Model access isn't just `model.field`** - `Model.helperFunction model` calls are also model dependencies that need extraction
   2. **Trace to destination** - We must follow data flow to see how values are actually used in views
   3. **Missed patterns** - We found duplicate logic like `currentStep == Just stepIdx` that was essentially re-implementing `vm.isStepCurrentlyPlaying`
   4. **View-optimized data** - ViewModel should serve how views consume data, not how model stores it

   ### Current ViewModel Pattern
   ```elm
   type alias ViewModel =
       { -- Computed constants (Model-dependent)
         totalSteps : Int
       , totalPitches : Int

       -- Simple computed props
       , canUndo : Bool
       , canRedo : Bool

       -- Helper functions (Model-dependent logic)
       , isStepCurrentlyPlaying : Int -> Bool
       , isPitchCellActive : PitchPos -> Bool
       , isPercCellActive : PercPos -> Bool

       -- Direct fields (last resort)
       -- (none yet - still have model access in views)
       }
   ```

   ### Migration Priority Order
   1. **First: Extract `Model.*` calls** - Functions like `Model.scaleConfig model`, `Model.getCurrentPlayingStep model`
   2. **Second: Trace destinations** - Follow where these values flow to understand usage patterns
   3. **Third: Create helpers** - For selection logic, formatting, complex computations
   4. **Last resort: Direct fields** - For simple field access like `model.bpm`

   ### Next Steps
   1. **Comprehensive audit** - Find ALL `Model.` and `model.` usage in Main.elm
   2. **Categorize patterns** - Group by usage type (selection, computation, formatting, etc.)
   3. **Plan extraction** - Design ViewModel additions based on actual usage patterns
   4. **Incremental migration** - One view function at a time
   5. **Goal: Zero model access** - Views only use ViewModel interface

   ### Key Insight
   **ViewModel design should be driven by view consumption patterns, not model structure.**
   The facade pattern creates a view-specific API that optimizes for how views actually use data,
   not how the model happens to store it.

   ---

   ## Operational Guide for Continuing Migration

   ### Systematic Discovery Process
   {-
   1. Find all candidates:
      - Run: grep -n "model\." src/Main.elm
      - Run: grep -n "Model\." src/Main.elm
   2. Categorize each usage (see patterns below)
   3. Group related extractions for batch migration
   -}

   ### Concrete Transformation Patterns
   {-
   PATTERN: Direct field access
   - Before: model.bpm
   - After: vm.bpm (add to ViewModel type)

   PATTERN: Model function call
   - Before: Model.scaleConfig model
   - After: vm.scaleConfig (add computed field)

   PATTERN: Selection logic
   - Before: model.currentScale == scale
   - After: vm.isScaleSelected scale (add helper function)

   PATTERN: Complex computation
   - Before: Scales.pitchIdxToNoteName pitchIdx (Model.scaleConfig model)
   - After: vm.pitchIdxToNoteName pitchIdx (add helper with pre-computed config)
   -}

   ### Step-by-Step Migration Workflow
   {-
   1. Pick one view function to migrate
   2. Identify all model access in that function
   3. Design ViewModel additions needed
   4. Add new fields/functions to ViewModel type in Model.elm
   5. Implement in toVm function
   6. Update function signature to accept ViewModel parameter
   7. Update all function calls throughout call chain
   8. Test compilation: elm make src/Main.elm --output=NUL
   9. Repeat for next function
   -}

   ### Validation Checklist
   {-
   After each migration:
   ✓ Compiles successfully
   ✓ No remaining model access in migrated function
   ✓ Function signature updated correctly
   ✓ All callers updated to pass vm parameter
   ✓ ViewModel type exports added to Model.elm
   -}

   ### Edge Cases & Common Pitfalls
   {-
   PITFALL: Don't miss indirect model access
   - Bad: currentStep == Just stepIdx (duplicates existing vm.isStepCurrentlyPlaying)
   - Good: vm.isStepCurrentlyPlaying stepIdx

   PITFALL: Don't expose raw complex data structures
   - Bad: vm.pitchGrid (exposes implementation)
   - Good: vm.isPitchCellActive : PitchPos -> Bool (encapsulates behavior)

   PITFALL: Don't create view-specific formatting in ViewModel
   - Bad: vm.bpmText (ViewModel shouldn't know about text formatting)
   - Good: vm.bpm (let view do String.fromInt vm.bpm)

   PITFALL: Don't mix pure module functions with model-dependent calls
   - Keep: Model.init, Model.startDrawingPitch (proper module functions)
   - Extract: Model.scaleConfig model, Model.getCurrentPlayingStep model (model-dependent)
   -}
-}
