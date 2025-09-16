port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- PORTS


port playNote : { note : Int, duration : Float, volume : Float } -> Cmd msg


port playSequence : List { note : Int, startTime : Float, duration : Float, volume : Float } -> Cmd msg


port timeSync : (Float -> msg) -> Sub msg



-- MODEL


type alias Model =
    { currentTime : Float
    , isPlaying : Bool
    , sequenceStartTime : Maybe Float
    }



-- UPDATE


type Msg
    = PlayNoteClicked Int
    | PlayMelodyClicked
    | TimeSync Float


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

        TimeSync currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )


twinkleTwinkleMelody : List { note : Int, startTime : Float, duration : Float, volume : Float }
twinkleTwinkleMelody =
    [ -- Verse 1: "Twinkle twinkle little star"
      { note = 60, startTime = 0.0, duration = 0.5, volume = 0.8 }   -- C: Twin-
    , { note = 60, startTime = 0.5, duration = 0.5, volume = 0.8 }   -- C: -kle
    , { note = 67, startTime = 1.0, duration = 0.5, volume = 0.8 }   -- G: twin-
    , { note = 67, startTime = 1.5, duration = 0.5, volume = 0.8 }   -- G: -kle
    , { note = 69, startTime = 2.0, duration = 0.5, volume = 0.8 }   -- A: lit-
    , { note = 69, startTime = 2.5, duration = 0.5, volume = 0.8 }   -- A: -tle
    , { note = 67, startTime = 3.0, duration = 1.0, volume = 0.8 }   -- G: star

    -- Verse 2: "How I wonder what you are"
    , { note = 65, startTime = 4.0, duration = 0.5, volume = 0.8 }   -- F: How
    , { note = 65, startTime = 4.5, duration = 0.5, volume = 0.8 }   -- F: I
    , { note = 64, startTime = 5.0, duration = 0.5, volume = 0.8 }   -- E: won-
    , { note = 64, startTime = 5.5, duration = 0.5, volume = 0.8 }   -- E: -der
    , { note = 62, startTime = 6.0, duration = 0.5, volume = 0.8 }   -- D: what
    , { note = 62, startTime = 6.5, duration = 0.5, volume = 0.8 }   -- D: you
    , { note = 60, startTime = 7.0, duration = 1.0, volume = 0.8 }   -- C: are

    -- Verse 3: "Up above the world so high"
    , { note = 67, startTime = 8.0, duration = 0.5, volume = 0.8 }   -- G: Up
    , { note = 67, startTime = 8.5, duration = 0.5, volume = 0.8 }   -- G: a-
    , { note = 65, startTime = 9.0, duration = 0.5, volume = 0.8 }   -- F: -bove
    , { note = 65, startTime = 9.5, duration = 0.5, volume = 0.8 }   -- F: the
    , { note = 64, startTime = 10.0, duration = 0.5, volume = 0.8 }  -- E: world
    , { note = 64, startTime = 10.5, duration = 0.5, volume = 0.8 }  -- E: so
    , { note = 62, startTime = 11.0, duration = 1.0, volume = 0.8 }  -- D: high

    -- Verse 4: "Like a diamond in the sky"
    , { note = 67, startTime = 12.0, duration = 0.5, volume = 0.8 }  -- G: Like
    , { note = 67, startTime = 12.5, duration = 0.5, volume = 0.8 }  -- G: a
    , { note = 65, startTime = 13.0, duration = 0.5, volume = 0.8 }  -- F: dia-
    , { note = 65, startTime = 13.5, duration = 0.5, volume = 0.8 }  -- F: -mond
    , { note = 64, startTime = 14.0, duration = 0.5, volume = 0.8 }  -- E: in
    , { note = 64, startTime = 14.5, duration = 0.5, volume = 0.8 }  -- E: the
    , { note = 62, startTime = 15.0, duration = 1.0, volume = 0.8 }  -- D: sky

    -- Final verse: "Twinkle twinkle little star"
    , { note = 60, startTime = 16.0, duration = 0.5, volume = 0.8 }  -- C: Twin-
    , { note = 60, startTime = 16.5, duration = 0.5, volume = 0.8 }  -- C: -kle
    , { note = 67, startTime = 17.0, duration = 0.5, volume = 0.8 }  -- G: twin-
    , { note = 67, startTime = 17.5, duration = 0.5, volume = 0.8 }  -- G: -kle
    , { note = 69, startTime = 18.0, duration = 0.5, volume = 0.8 }  -- A: lit-
    , { note = 69, startTime = 18.5, duration = 0.5, volume = 0.8 }  -- A: -tle
    , { note = 67, startTime = 19.0, duration = 1.0, volume = 0.8 }  -- G: star

    -- Final line: "How I wonder what you are"
    , { note = 65, startTime = 20.0, duration = 0.5, volume = 0.8 }  -- F: How
    , { note = 65, startTime = 20.5, duration = 0.5, volume = 0.8 }  -- F: I
    , { note = 64, startTime = 21.0, duration = 0.5, volume = 0.8 }  -- E: won-
    , { note = 64, startTime = 21.5, duration = 0.5, volume = 0.8 }  -- E: -der
    , { note = 62, startTime = 22.0, duration = 0.5, volume = 0.8 }  -- D: what
    , { note = 62, startTime = 22.5, duration = 0.5, volume = 0.8 }  -- D: you
    , { note = 60, startTime = 23.0, duration = 1.5, volume = 0.8 }  -- C: are (final hold)
    ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "p-6 bg-gray-50 min-h-screen" ]
        [ h1 [ class "text-3xl font-bold mb-6 text-gray-800 text-center" ]
            [ text "Song Maker Piano" ]

        -- Debug: Show current time
        , div [ class "text-center mb-4 text-sm text-gray-600" ]
            [ text ("Audio Time: " ++ String.fromFloat model.currentTime) ]

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
        { init = \_ -> ( { currentTime = 0.0, isPlaying = False, sequenceStartTime = Nothing }, Cmd.none )
        , update = update
        , subscriptions = \_ -> timeSync TimeSync
        , view = view
        }
