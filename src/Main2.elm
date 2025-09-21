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
        stepCount =
            16

        noteCount =
            8
    in
    div
        [ class "grid bg-neutral-800 border border-neutral-700 w-max h-max min-w-full min-h-full"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt (stepCount + 1) ++ ", minmax(48px, 1fr))")
        , style "grid-template-rows" ("minmax(32px, auto) repeat(" ++ String.fromInt noteCount ++ ", minmax(32px, 1fr)) 40px 40px")
        ]
        ([ div [ class "bg-neutral-700" ] [] -- Empty corner cell
         ]
            ++ -- Step headers
               List.map viewStepHeader (List.range 0 (stepCount - 1))
            ++ -- Note rows
               (List.range 0 (noteCount - 1)
                    |> List.concatMap (viewNoteRow stepCount)
               )
            ++ -- Percussion rows
               viewPercussionRows stepCount
        )


viewStepHeader : Int -> Html Msg
viewStepHeader stepIndex =
    div
        [ class "bg-neutral-600 border-b border-neutral-500 flex items-center justify-center text-xs font-bold text-neutral-300"
        ]
        [ text (String.fromInt (stepIndex + 1)) ]


viewNoteRow : Int -> Int -> List (Html Msg)
viewNoteRow stepCount noteIndex =
    [ div
        [ class "bg-neutral-700 border-r border-neutral-600 flex items-center justify-center text-xs font-bold text-neutral-300"
        ]
        [ text ("Note " ++ String.fromInt (noteIndex + 1)) ]
    ]
        ++ List.map (viewNoteCell noteIndex) (List.range 0 (stepCount - 1))


viewNoteCell : Int -> Int -> Html Msg
viewNoteCell noteIndex stepIndex =
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
        ++ List.map (viewPercussionCell "kick") (List.range 0 (stepCount - 1))
        ++ [ -- Snare drum label
             div
                [ class "bg-orange-800 border-r border-neutral-600 flex items-center justify-center text-xs font-bold text-white"
                ]
                [ text "Snare" ]
           ]
        ++ List.map (viewPercussionCell "snare") (List.range 0 (stepCount - 1))


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
        [ class (bgClass ++ " border-r border-b border-neutral-600 cursor-pointer transition-colors")
        ]
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



-- View Helpers


px f =
    String.fromFloat f ++ "px"
