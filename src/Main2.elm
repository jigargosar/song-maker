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
    div [ class "min-h-screen bg-neutral-900 text-white flex flex-col" ]
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
                , div [ class "bg-accent-700 hover:bg-fuchsia-800 text-white font-bold py-2 px-6 rounded-lg transition-colors cursor-pointer" ]
                    [ text "Play" ]
                ]
            ]
        ]


centerView : Html Msg
centerView =
    div [ class "flex-1 bg-neutral-900 p-6" ]
        [ div [ class "max-w-full mx-auto" ]
            [ div [ class "bg-neutral-800 rounded-lg p-8 border border-neutral-700" ]
                [ div [ class "text-center" ]
                    [ div [ class "text-6xl mb-4" ] [ text "🎵" ]
                    , div [ class "text-2xl font-semibold text-neutral-300 mb-4" ]
                        [ text "Grid Coming Soon" ]
                    , div [ class "text-neutral-400" ]
                        [ text "Building grid-first architecture with percussion support" ]
                    ]
                ]
            ]
        ]


footerView : Html Msg
footerView =
    div [ class "bg-neutral-800 border-t border-neutral-700 px-6 py-3" ]
        [ div [ class "flex items-center justify-between text-sm text-neutral-400" ]
            [ div [] [ text "Ready to rock 🎸" ]
            , div [] [ text "Time: 0.0s" ]
            , div [] [ text "V2 - Clean Architecture" ]
            ]
        ]
