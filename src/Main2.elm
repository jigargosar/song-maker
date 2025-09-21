module Main2 exposing (main)

import Browser
import Html as H exposing (Html, div, text)
import Html.Attributes as HA exposing (class, style)


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


type alias Model =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- Update


type Msg
    = NoOp


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "h-screen bg-neutral-900 text-white flex flex-col" ]
        [ headerView
        , centerView
        , footerView
        ]


headerView : Html Msg
headerView =
    div [ class "bg-neutral-800 border-b border-neutral-700 px-6 py-4" ]
        [ div [ class "flex items-center justify-between" ]
            [ div [ class "text-2xl font-bold text-white" ]
                [ text "Song Maker V2" ]
            , div [ class "flex items-center gap-4" ]
                [ div [ class "text-neutral-300 text-sm" ]
                    [ text "BPM: 120" ]
                , div [ class "bg-fuchsia-700 hover:bg-fuchsia-800 text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer" ]
                    [ text "Play" ]
                ]
            ]
        ]


centerView : Html Msg
centerView =
    div [ class "flex-1 overflow-auto" ]
        [ viewGrid ]


viewGrid : Html Msg
viewGrid =
    let
        totalStepColumns =
            16

        totalPitchRows =
            8

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
        [ class "grid bg-neutral-800 border border-neutral-700 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" gridTemplateColumns
        , style "grid-template-rows" gridTemplateRows
        ]
        ([ {- Empty corner cell -} div [ class "bg-neutral-700" ] [] ]
            ++ {- Step headers row -} times viewStepHeader totalStepColumns
            ++ {- Pitch rows -} (times (viewPitchRow totalStepColumns) totalPitchRows |> List.concat)
            ++ {- Percussion rows -} viewPercussionRows totalStepColumns
        )


viewStepHeader : Int -> Html Msg
viewStepHeader stepIndex =
    div
        [ class "bg-neutral-600 flex items-center justify-center text-xs font-bold text-neutral-200" ]
        [ text (String.fromInt (stepIndex + 1)) ]


viewPitchRow : Int -> Int -> List (Html Msg)
viewPitchRow stepCount pitchIndex =
    let
        viewPitchLabel =
            div
                [ class "bg-neutral-600 flex items-center justify-center text-xs font-bold text-neutral-300" ]
                [ text ("Pitch " ++ String.fromInt (pitchIndex + 1)) ]
    in
    viewPitchLabel :: times (viewPitchCell pitchIndex) stepCount


viewPitchCell : Int -> Int -> Html Msg
viewPitchCell pitchIndex stepIndex =
    div
        [ class "bg-neutral-800 hover:bg-neutral-700 border-r border-b border-neutral-600 cursor-pointer transition-colors"
        ]
        []


viewPercussionRows : Int -> List (Html Msg)
viewPercussionRows stepCount =
    [ -- Kick drum label
      div
        [ class "bg-red-800 border-r border-neutral-600 flex items-center justify-center text-xs font-bold text-white"
        ]
        [ text "Kick" ]
    ]
        ++ times (viewPercussionCell "kick") stepCount
        ++ [ -- Snare drum label
             div
                [ class "bg-orange-800 border-r border-neutral-600 flex items-center justify-center text-xs font-bold text-white"
                ]
                [ text "Snare" ]
           ]
        ++ times (viewPercussionCell "snare") stepCount


viewPercussionCell : String -> Int -> Html Msg
viewPercussionCell drumType stepIndex =
    let
        bgClass =
            if drumType == "kick" then
                "bg-red-900 hover:bg-red-800"

            else
                "bg-orange-900 hover:bg-orange-800"
    in
    div
        [ class bgClass, class "border-r border-b border-neutral-600 cursor-pointer transition-colors" ]
        []


footerView : Html Msg
footerView =
    div [ class "bg-neutral-800 border-t border-neutral-700 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-neutral-400" ]
            [ div [] [ text "Ready to rock 🎸" ]
            , div [] [ text "Time: 0.0s" ]
            , div [] [ text "V2 - Clean Architecture" ]
            ]
        ]



-- Basic Helpers


times fn i =
    List.range 0 (i - 1) |> List.map fn



-- View Helpers


px f =
    String.fromFloat f ++ "px"
