port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- PORTS


port playNote : { note : Int, duration : Float, volume : Float } -> Cmd msg


port playSequence : List { note : Int, startTime : Float, duration : Float, volume : Float } -> Cmd msg



-- MODEL


type alias Model =
    {}



-- UPDATE


type Msg
    = PlayNoteClicked Int
    | PlayMelodyClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayNoteClicked midiNote ->
            ( model
            , playNote { note = midiNote, duration = 0.6, volume = 0.8 }
            )

        PlayMelodyClicked ->
            ( model
            , playSequence twinkleTwinkleMelody
            )


twinkleTwinkleMelody : List { note : Int, startTime : Float, duration : Float, volume : Float }
twinkleTwinkleMelody =
    [ { note = 60, startTime = 0.0, duration = 0.5, volume = 0.8 }   -- C
    , { note = 60, startTime = 0.5, duration = 0.5, volume = 0.8 }   -- C
    , { note = 67, startTime = 1.0, duration = 0.5, volume = 0.8 }   -- G
    , { note = 67, startTime = 1.5, duration = 0.5, volume = 0.8 }   -- G
    , { note = 69, startTime = 2.0, duration = 0.5, volume = 0.8 }   -- A
    , { note = 69, startTime = 2.5, duration = 0.5, volume = 0.8 }   -- A
    , { note = 67, startTime = 3.0, duration = 1.0, volume = 0.8 }   -- G
    , { note = 65, startTime = 4.0, duration = 0.5, volume = 0.8 }   -- F
    , { note = 65, startTime = 4.5, duration = 0.5, volume = 0.8 }   -- F
    , { note = 64, startTime = 5.0, duration = 0.5, volume = 0.8 }   -- E
    , { note = 64, startTime = 5.5, duration = 0.5, volume = 0.8 }   -- E
    , { note = 62, startTime = 6.0, duration = 0.5, volume = 0.8 }   -- D
    , { note = 62, startTime = 6.5, duration = 0.5, volume = 0.8 }   -- D
    , { note = 60, startTime = 7.0, duration = 1.0, volume = 0.8 }   -- C
    ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "p-6 bg-gray-50 min-h-screen" ]
        [ h1 [ class "text-3xl font-bold mb-6 text-gray-800 text-center" ]
            [ text "Song Maker Piano" ]

        -- Play Melody Button
        , div [ class "text-center mb-6" ]
            [ button
                [ class "bg-indigo-600 hover:brightness-110 text-white font-bold py-3 px-8 rounded-lg shadow-md transition-all"
                , onClick PlayMelodyClicked
                ]
                [ text "Play Melody" ]
            ]

        -- Octave 3
        , octaveSection "Octave 3" 48 [ "bg-blue-500", "bg-green-500", "bg-yellow-500", "bg-orange-500", "bg-red-500", "bg-purple-500", "bg-pink-500" ]

        -- Octave 4 (Middle C)
        , octaveSection "Octave 4 (Middle C)" 60 [ "bg-blue-600", "bg-green-600", "bg-yellow-600", "bg-orange-600", "bg-red-600", "bg-purple-600", "bg-pink-600" ]

        -- Octave 5
        , octaveSection "Octave 5" 72 [ "bg-blue-400", "bg-green-400", "bg-yellow-400", "bg-orange-400", "bg-red-400", "bg-purple-400", "bg-pink-400" ]
        ]


octaveSection : String -> Int -> List String -> Html Msg
octaveSection title startNote colors =
    let
        noteNames =
            [ "C", "D", "E", "F", "G", "A", "B" ]

        noteOffsets =
            [ 0, 2, 4, 5, 7, 9, 11 ]

        octaveNumber =
            String.fromInt ((startNote + 12) // 12)
    in
    div [ class "mb-6" ]
        [ h3 [ class "text-lg font-semibold mb-3 text-gray-700" ]
            [ text title ]
        , div [ class "flex gap-2 flex-wrap" ]
            (List.map3
                (\noteName offset color ->
                    let
                        midiNote =
                            startNote + offset

                        noteLabel =
                            noteName ++ octaveNumber

                        buttonClass =
                            color ++ " text-white font-medium py-3 px-6 rounded-lg shadow-md transition-all hover:brightness-110"

                        specialClass =
                            if midiNote == 60 then
                                " border-2 border-blue-800"

                            else
                                ""
                    in
                    button
                        [ class (buttonClass ++ specialClass)
                        , onClick (PlayNoteClicked midiNote)
                        ]
                        [ text noteLabel ]
                )
                noteNames
                noteOffsets
                colors
            )
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( {}, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
