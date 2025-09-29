port module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Grid exposing (PercGrid, PercPos, PitchGrid, PitchPos)
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE
import Instruments exposing (DrumKit, PercType, TonalInstrument)
import Json.Decode as JD
import Model exposing (DrawState(..), HistoryState, Model, PlayState(..))
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Timing exposing (TimeConfig)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Url.Query.Pipeline as Pipeline
import Utils exposing (..)


{-| <https://surikov.github.io/webaudiofont/>
-}



-- PORTS


port playNote : { webAudioFont : String, midi : Int, duration : Float, volume : Float } -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- Query parameter record for pipeline parsing


type alias QueryParams =
    { bpm : Maybe Int
    , octaveStart : Maybe Int
    , octaveCount : Maybe Int
    }


queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Pipeline.succeed QueryParams
        |> Pipeline.optional (Query.int "bpm")
        |> Pipeline.optional (Query.int "octaveStart")
        |> Pipeline.optional (Query.int "octaveCount")


buildQuery : Model -> String
buildQuery model =
    UB.absolute
        []
        [ UB.int "bpm" model.bpm
        , UB.int "octaveStart" model.octaveStart
        , UB.int "octaveCount" model.octaveCount
        ]


parseQueryParams : Url -> Maybe QueryParams
parseQueryParams url =
    if url.query == Nothing then
        -- This check is required since when there is no query params we need to apply default params
        Nothing

    else
        Parser.parse (Parser.top <?> queryParser) url
            |> Maybe.andThen identity


applyQueryDefaults : Model -> Model
applyQueryDefaults model =
    { model
        | bpm = 120
        , octaveStart = 3
        , octaveCount = 3
    }


applyQueryParams : Url -> Model -> Model
applyQueryParams url model =
    case parseQueryParams url of
        Just params ->
            { model
                | bpm = Maybe.withDefault model.bpm params.bpm
                , octaveStart = Maybe.withDefault model.octaveStart params.octaveStart
                , octaveCount = Maybe.withDefault model.octaveCount params.octaveCount
            }

        Nothing ->
            applyQueryDefaults model



{-
   parseQueryStringToDict : String -> Dict String String
   parseQueryStringToDict queryString =
       if String.isEmpty queryString then
           Dict.empty
       else
           queryString
               |> String.split "&"
               |> List.filterMap
                   (\param ->
                       case String.split "=" param of
                           [ key, value ] ->
                               Maybe.map2 (\k v -> ( k, v ))
                                   (Url.percentDecode key)
                                   (Url.percentDecode value)
                           [ key ] ->
                               Maybe.map (\k -> ( k, "" )) (Url.percentDecode key)
                           _ ->
                               Nothing
                   )
               |> Dict.fromList


   areQueryStringsEqual : String -> String -> Bool
   areQueryStringsEqual queryString1 queryString2 =
       let
           normalizeQuery query =
               query
                   |> String.dropLeft (if String.startsWith "?" query then 1 else 0)
                   |> parseQueryStringToDict
       in
       normalizeQuery queryString1 == normalizeQuery queryString2
-}
-- Standard snare perc MIDI note


type alias Flags =
    {}


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



-- Model
-- (Note: Grid types imported from Grid module)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initialModel : Model
        initialModel =
            Model.init url key
    in
    ( initialModel
        |> applyQueryParams url
        |> applySong twinkleSong
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



-- History management functions


{-| Push the current state to the undo stack and clear the redo stack
-}
pushToHistory : Model -> Model
pushToHistory model =
    let
        currentHistoryState : HistoryState
        currentHistoryState =
            { pitchGrid = model.pitchGrid
            , percGrid = model.percGrid
            , scaleType = model.scaleType
            , rootNote = model.rootNote
            , octaveStart = model.octaveStart
            , octaveCount = model.octaveCount
            , bars = model.bars
            , beatsPerBar = model.beatsPerBar
            , subdivisions = model.subdivisions
            }

        newUndoStack =
            currentHistoryState :: model.undoStack

        -- Clear redo stack when new operation is performed
        newRedoStack =
            []
    in
    { model
        | undoStack = newUndoStack
        , redoStack = newRedoStack
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDrawingPitch position ->
            case model.drawState of
                NotDrawing ->
                    let
                        modelWithHistory =
                            Model.pushToHistory model

                        currentlyActive =
                            Grid.isPitchCellActive position model.pitchGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPitch

                            else
                                DrawingPitch
                    in
                    ( { modelWithHistory
                        | drawState = newDrawState
                        , pitchGrid = Grid.updatePitchCell position (not currentlyActive) modelWithHistory.pitchGrid
                      }
                    , playPitchCmdIf (not currentlyActive) position.pitchIdx modelWithHistory
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPitch position ->
            case model.drawState of
                DrawingPitch ->
                    ( { model | pitchGrid = Grid.updatePitchCell position True model.pitchGrid }
                    , playPitchCmdIf True position.pitchIdx model
                    )

                ErasingPitch ->
                    ( { model | pitchGrid = Grid.updatePitchCell position False model.pitchGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopDrawing ->
            ( { model | drawState = NotDrawing }, Cmd.none )

        StartDrawingPerc position ->
            case model.drawState of
                NotDrawing ->
                    let
                        modelWithHistory =
                            Model.pushToHistory model

                        currentlyActive =
                            Grid.isPercCellActive position model.percGrid

                        newDrawState =
                            if currentlyActive then
                                ErasingPerc

                            else
                                DrawingPerc
                    in
                    ( { modelWithHistory
                        | drawState = newDrawState
                        , percGrid = Grid.updatePercCell position (not currentlyActive) modelWithHistory.percGrid
                      }
                    , playPercCmdIf (not currentlyActive) position.percType modelWithHistory
                    )

                _ ->
                    ( model, Cmd.none )

        ContinueDrawingPerc position ->
            case model.drawState of
                DrawingPerc ->
                    ( { model | percGrid = Grid.updatePercCell position True model.percGrid }
                    , playPercCmdIf True position.percType model
                    )

                ErasingPerc ->
                    ( { model | percGrid = Grid.updatePercCell position False model.percGrid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayPitchNote pitchIdx ->
            ( model, playPitchCmdIf True pitchIdx model )

        PlayPercNote percType ->
            ( model, playPercCmdIf True percType model )

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
                            Timing.noteDuration (Model.timeConfig model)

                        currentStep =
                            floor (elapsedTime / duration)
                    in
                    if currentStep >= nextStep then
                        let
                            stepToSchedule =
                                modBy (Timing.getTotalSteps (Model.timeConfig model)) nextStep

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

        ChangeScaleType newScaleType ->
            let
                modelWithHistory =
                    Model.pushToHistory model

                newModel =
                    { modelWithHistory | scaleType = newScaleType }

                newPitchGrid =
                    Grid.resizePitchGrid (Model.scaleConfig modelWithHistory) (Model.scaleConfig newModel) (Model.timeConfig newModel) modelWithHistory.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeRootNote newRootNote ->
            let
                modelWithHistory =
                    Model.pushToHistory model

                newModel =
                    { modelWithHistory | rootNote = newRootNote }

                newPitchGrid =
                    Grid.transposePitchGrid (Model.scaleConfig modelWithHistory) (Model.scaleConfig newModel) modelWithHistory.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeOctaveStart newStart ->
            let
                modelWithHistory =
                    Model.pushToHistory model

                clampedStart =
                    max 1 newStart

                newModel =
                    { modelWithHistory | octaveStart = clampedStart }

                newPitchGrid =
                    Grid.resizePitchGrid (Model.scaleConfig modelWithHistory) (Model.scaleConfig newModel) (Model.timeConfig newModel) modelWithHistory.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeOctaveCount newCount ->
            let
                modelWithHistory =
                    Model.pushToHistory model

                clampedCount =
                    max 1 newCount

                newModel =
                    { modelWithHistory | octaveCount = clampedCount }

                newPitchGrid =
                    Grid.resizePitchGrid (Model.scaleConfig modelWithHistory) (Model.scaleConfig newModel) (Model.timeConfig newModel) modelWithHistory.pitchGrid
            in
            ( { newModel | pitchGrid = newPitchGrid }
            , Cmd.none
            )

        ChangeBPM newBPM ->
            ( { model | bpm = max 1 newBPM }
            , Cmd.none
            )

        ChangeTonalInstrument newInstrument ->
            ( { model | currentTonalInstrument = newInstrument }
            , Cmd.none
            )

        ChangeDrumKit newDrumKit ->
            ( { model | currentDrumKit = newDrumKit }
            , Cmd.none
            )

        ChangeBars bars ->
            let
                modelWithHistory =
                    Model.pushToHistory model
            in
            ( { modelWithHistory | bars = atLeast 1 bars }
            , Cmd.none
            )

        ChangeBeatsPerBar newBeatsPerBar ->
            let
                modelWithHistory =
                    Model.pushToHistory model
            in
            ( { modelWithHistory | beatsPerBar = atLeast 1 newBeatsPerBar }
            , Cmd.none
            )

        ChangeSubdivisions subDivisions ->
            let
                modelWithHistory =
                    Model.pushToHistory model
            in
            ( { modelWithHistory | subdivisions = atLeast 1 subDivisions }
            , Cmd.none
            )

        Undo ->
            case model.undoStack of
                [] ->
                    ( model, Cmd.none )

                lastState :: remainingUndoStack ->
                    let
                        newRedoStack : List HistoryState
                        newRedoStack =
                            { pitchGrid = model.pitchGrid
                            , percGrid = model.percGrid
                            , scaleType = model.scaleType
                            , rootNote = model.rootNote
                            , octaveStart = model.octaveStart
                            , octaveCount = model.octaveCount
                            , bars = model.bars
                            , beatsPerBar = model.beatsPerBar
                            , subdivisions = model.subdivisions
                            }
                                :: model.redoStack

                        newModel =
                            { model
                                | pitchGrid = lastState.pitchGrid
                                , percGrid = lastState.percGrid
                                , scaleType = lastState.scaleType
                                , rootNote = lastState.rootNote
                                , octaveStart = lastState.octaveStart
                                , octaveCount = lastState.octaveCount
                                , bars = lastState.bars
                                , beatsPerBar = lastState.beatsPerBar
                                , subdivisions = lastState.subdivisions
                                , undoStack = remainingUndoStack
                                , redoStack = newRedoStack
                            }
                    in
                    ( newModel, Cmd.none )

        Redo ->
            case model.redoStack of
                [] ->
                    ( model, Cmd.none )

                lastState :: remainingRedoStack ->
                    let
                        newUndoStack =
                            { pitchGrid = model.pitchGrid
                            , percGrid = model.percGrid
                            , scaleType = model.scaleType
                            , rootNote = model.rootNote
                            , octaveStart = model.octaveStart
                            , octaveCount = model.octaveCount
                            , bars = model.bars
                            , beatsPerBar = model.beatsPerBar
                            , subdivisions = model.subdivisions
                            }
                                :: model.undoStack

                        newModel =
                            { model
                                | pitchGrid = lastState.pitchGrid
                                , percGrid = lastState.percGrid
                                , scaleType = lastState.scaleType
                                , rootNote = lastState.rootNote
                                , octaveStart = model.octaveStart
                                , octaveCount = model.octaveCount
                                , bars = lastState.bars
                                , beatsPerBar = lastState.beatsPerBar
                                , subdivisions = lastState.subdivisions
                                , undoStack = newUndoStack
                                , redoStack = remainingRedoStack
                            }
                    in
                    ( newModel, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( applyQueryParams url { model | url = url }, Cmd.none )

        Save ->
            ( model
            , let
                query =
                    buildQuery model
              in
              if model.url.query == Just query then
                Cmd.none

              else
                Nav.pushUrl model.key query
            )



-- View


view : Model -> Document Msg
view model =
    { title = "SM"
    , body =
        [ div [ class "h-screen bg-gray-900 text-white flex flex-col select-none" ]
            [ viewHeader model
            , centerView model
            , footerView model
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


centerView : Model -> Html Msg
centerView model =
    div [ class "flex-1 overflow-auto" ] [ viewGrid model ]


viewGrid : Model -> Html Msg
viewGrid model =
    let
        currentStep =
            getCurrentPlayingStep model

        totalSteps =
            Timing.getTotalSteps (Model.timeConfig model)

        gridTemplateCols =
            format "minmax($pitchLabelColMinWidth, auto) repeat($totalSteps, minmax($stepColMinWidth, 1fr))"
                [ ( "$pitchLabelColMinWidth", px 48 )
                , ( "$totalSteps", String.fromInt totalSteps )
                , ( "$stepColMinWidth", px 48 )
                ]

        gridTemplateRows =
            format "minmax($stepLabelRowMinHeight, auto) repeat($totalPitches, minmax($pitchRowMinHeight, 1fr)) repeat(2, $percRowHeight)"
                [ ( "$stepLabelRowMinHeight", px 32 )
                , ( "$totalPitches", String.fromInt (Scales.getTotalPitches (Model.scaleConfig model)) )
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
            ++ {- Step Labels row -} times (\stepIdx -> viewStepLabel currentStep stepIdx) totalSteps
            ++ {- Pitch rows -} (times (\pitchIdx -> viewPitchRow model model.pitchGrid currentStep pitchIdx) (Scales.getTotalPitches (Model.scaleConfig model)) |> List.concat)
            ++ {- Perc Snare row -} viewPercRow Instruments.percSnare totalSteps model.percGrid currentStep
            ++ {- Perc Kick row -} viewPercRow Instruments.percKick totalSteps model.percGrid currentStep
        )


viewStepLabel : Maybe Int -> Int -> Html Msg
viewStepLabel currentStep stepIdx =
    let
        isCurrentStep =
            currentStep == Just stepIdx

        bgClass =
            if isCurrentStep then
                accentBgColor

            else
                labelBgColor
    in
    div
        [ class labelClass, class bgClass, class "border-b border-gray-600" ]
        [ text (String.fromInt (stepIdx + 1)) ]


viewPitchRow : Model -> PitchGrid -> Maybe Int -> Int -> List (Html Msg)
viewPitchRow model pitchGrid currentStep pitchIdx =
    let
        viewPitchLabel =
            div
                [ class labelBgColorAndClass, class "border-[0.5px]" ]
                [ text (Scales.pitchIdxToNoteName pitchIdx (Model.scaleConfig model)) ]
    in
    -- TODO: Should we fix function parameters?
    viewPitchLabel :: times (\stepIdx -> viewPitchCell pitchIdx pitchGrid currentStep stepIdx) (Timing.getTotalSteps (Model.timeConfig model))


viewPitchCell : Int -> PitchGrid -> Maybe Int -> Int -> Html Msg
viewPitchCell pitchIdx pitchGrid currentStep stepIdx =
    let
        position =
            { pitchIdx = pitchIdx, stepIdx = stepIdx }

        isActive =
            Grid.isPitchCellActive position pitchGrid

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


footerView : Model -> Html Msg
footerView model =
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
                , HA.disabled (List.isEmpty model.undoStack)
                ]
                [ text "↶ Undo" ]
            , H.button
                [ class "bg-gray-700 text-white px-3 py-1 rounded disabled:opacity-50"
                , HE.onClick Redo
                , HA.disabled (List.isEmpty model.redoStack)
                ]
                [ text "↷ Redo" ]
            , H.button
                [ class "bg-gray-700 text-white px-3 py-1 rounded hover:bg-gray-600 transition-colors"
                , HE.onClick Save
                ]
                [ text "Save" ]
            ]
        ]



-- Sequencer Functions


getCurrentPlayingStep : Model -> Maybe Int
getCurrentPlayingStep model =
    case model.playState of
        PlayingStarted _ ->
            Just 0

        Playing { nextStep } ->
            Just (modBy (Timing.getTotalSteps (Model.timeConfig model)) (nextStep - 1))

        Stopped ->
            Nothing


type alias NoteToPlay =
    { webAudioFont : String, midi : Int, duration : Float, volume : Float }


getActiveNotesForStep : Int -> Model -> List NoteToPlay
getActiveNotesForStep stepIdx model =
    let
        duration =
            Timing.noteDuration (Model.timeConfig model)

        pitchNotes =
            times
                (\pitchIdx ->
                    let
                        position =
                            { pitchIdx = pitchIdx, stepIdx = stepIdx }
                    in
                    if Grid.isPitchCellActive position model.pitchGrid then
                        Just
                            { webAudioFont = Instruments.tonalWebAudioFont model.currentTonalInstrument
                            , midi = Scales.pitchIdxToMidi pitchIdx (Model.scaleConfig model)
                            , duration = duration
                            , volume = 0.7
                            }

                    else
                        Nothing
                )
                (Scales.getTotalPitches (Model.scaleConfig model))
                |> List.filterMap identity

        drumConfig =
            Instruments.drumKitConfig model.currentDrumKit

        percNotes =
            Instruments.allPercTypes
                |> List.filterMap
                    (\percType ->
                        let
                            position =
                                { percType = percType, stepIdx = stepIdx }

                            ( webAudioFontName, midiNote ) =
                                case percType of
                                    _ ->
                                        if percType == Instruments.percKick then
                                            ( drumConfig.kickWebAudioFont, drumConfig.kickMidi )

                                        else
                                            ( drumConfig.snareWebAudioFont, drumConfig.snareMidi )
                        in
                        if Grid.isPercCellActive position model.percGrid then
                            Just
                                { webAudioFont = webAudioFontName
                                , midi = midiNote
                                , duration = duration
                                , volume = 0.8
                                }

                        else
                            Nothing
                    )
    in
    pitchNotes ++ percNotes



-- Audio Helper Functions


playPitchCmdIf : Bool -> Int -> Model -> Cmd Msg
playPitchCmdIf shouldPlay pitchIdx model =
    if shouldPlay then
        playNote
            { webAudioFont = Instruments.tonalWebAudioFont model.currentTonalInstrument
            , midi = Scales.pitchIdxToMidi pitchIdx (Model.scaleConfig model)
            , duration = 0.5
            , volume = 0.7
            }

    else
        Cmd.none


playPercCmdIf : Bool -> PercType -> Model -> Cmd Msg
playPercCmdIf shouldPlay percType model =
    if shouldPlay then
        let
            drumConfig =
                Instruments.drumKitConfig model.currentDrumKit

            ( webAudioFontName, midiNote ) =
                case percType of
                    _ ->
                        if percType == Instruments.percKick then
                            ( drumConfig.kickWebAudioFont, drumConfig.kickMidi )

                        else
                            ( drumConfig.snareWebAudioFont, drumConfig.snareMidi )
        in
        playNote
            { webAudioFont = webAudioFontName
            , midi = midiNote
            , duration = 0.5
            , volume = 0.8
            }

    else
        Cmd.none


type alias SongConfig =
    { melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , bpm : Int
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }


twinkleSong : SongConfig
twinkleSong =
    let
        kick =
            Instruments.percKick

        snare =
            Instruments.percSnare
    in
    { melody =
        -- "Twinkle twinkle little star"
        [ [ "C4", "C3", "E4" ], [ "C4" ], [ "G4", "G3", "B4" ], [ "G4" ] ]
            -- "how I wonder what"
            ++ [ [ "A4", "F3", "C5" ], [ "A4" ], [ "G4", "G3" ], [] ]
            -- "what you are so"
            ++ [ [ "F4", "F3", "A4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "E4" ] ]
            -- "far above the world"
            ++ [ [ "D4", "G3", "B4" ], [ "D4" ], [ "C4", "C3", "E4" ], [] ]
            -- "Up above the world"
            ++ [ [ "G4", "G3", "B4" ], [ "G4" ], [ "F4", "F3", "A4" ], [ "F4" ] ]
            -- "so high like a"
            ++ [ [ "E4", "C3", "G4" ], [ "E4" ], [ "D4", "G3", "B4" ], [ "D4" ] ]
            -- "diamond in the"
            ++ [ [ "G4", "C3", "E4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "D4" ] ]
            -- "sky (end)"
            ++ [ [ "C4", "C3", "E4" ], [], [], [] ]
    , percussion =
        -- "Twinkle twinkle little star"
        [ [ kick ], [], [ snare ], [] ]
            -- "how I wonder what"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "what you are so"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "far above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "Up above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "so high like a"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "diamond in the"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "sky (end)"
            ++ [ [ kick ], [], [ snare ], [] ]
    , bpm = 90
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


applySong : SongConfig -> Model -> Model
applySong sc model =
    { model
        | pitchGrid = Grid.convertMelodyToGrid sc.melody (Model.scaleConfig model)
        , percGrid = Grid.convertPercussionToGrid sc.percussion
        , bpm = sc.bpm
        , octaveStart = sc.octaveStart
        , octaveCount = sc.octaveCount
        , bars = sc.bars
        , beatsPerBar = sc.beatsPerBar
        , subdivisions = sc.subdivisions
    }



-- Cell State Management
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
