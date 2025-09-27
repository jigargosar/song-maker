module QPP exposing (main)

import Browser
import Dict
import Html exposing (Html, div, text, pre)
import Html.Attributes exposing (style)
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Url.Query.Pipeline as Pipeline


-- Example types for demonstration
type Instrument
    = Piano
    | Guitar
    | Flute

type DrumType
    = Acoustic
    | Electronic

type alias QueryParams =
    { instrument : Maybe Instrument
    , drums : Maybe DrumType
    , bpm : Maybe Int
    , name : Maybe String
    }

-- Query parser using elm-url-query-pipeline
queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Pipeline.succeed QueryParams
        |> Pipeline.optional (Query.enum "instrument"
            (Dict.fromList
                [ ( "piano", Piano )
                , ( "guitar", Guitar )
                , ( "flute", Flute )
                ]
            ))
        |> Pipeline.optional (Query.enum "drums"
            (Dict.fromList
                [ ( "acoustic", Acoustic )
                , ( "electronic", Electronic )
                ]
            ))
        |> Pipeline.optional (Query.int "bpm")
        |> Pipeline.optional (Query.string "name")

-- Helper function to parse from query string
parseQueryString : String -> Maybe QueryParams
parseQueryString queryString =
    case Url.fromString ("http://example.com" ++ queryString) of
        Just url ->
            Parser.parse (Parser.top <?> queryParser) url
                |> Maybe.withDefault Nothing
        Nothing ->
            Nothing

-- Example usage
main : Html msg
main =
    let
        exampleQueries =
            [ "?instrument=piano&bpm=120&name=MySong"
            , "?instrument=guitar&drums=acoustic"
            , "?drums=electronic&bpm=140"
            , "?name=TestSong&instrument=flute&bpm=90"
            ]

        parseExample query =
            let
                parsed = parseQueryString query
            in
            div [ style "margin" "10px", style "padding" "10px", style "border" "1px solid #ccc" ]
                [ div [] [ text ("Query: " ++ query) ]
                , div [] [ text ("Parsed: " ++ Debug.toString parsed) ]
                ]
    in
    div [ style "padding" "20px", style "font-family" "monospace" ]
        [ div [ style "font-size" "18px", style "font-weight" "bold", style "margin-bottom" "20px" ]
            [ text "elm-url-query-pipeline Demo" ]
        , div [] (List.map parseExample exampleQueries)
        , div [ style "margin-top" "20px", style "background" "#f0f0f0", style "padding" "15px" ]
            [ text "Usage Pattern:"
            , pre [ style "margin" "10px 0" ]
                [ text """Pipeline.succeed RecordConstructor
    |> Pipeline.optional (Query.enum "param" [(\"value\", Type)])
    |> Pipeline.optional (Query.int "number")
    |> Pipeline.optional (Query.string "text")""" ]
            ]
        ]
