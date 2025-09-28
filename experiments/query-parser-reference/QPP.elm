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

-- Query parser using elm-url-query-pipeline (all optional)
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

-- Alternative: Query parser with required parameters
type alias RequiredQueryParams =
    { instrument : Instrument
    , bpm : Int
    , drums : Maybe DrumType
    , name : Maybe String
    }

requiredQueryParser : Query.Parser (Maybe RequiredQueryParams)
requiredQueryParser =
    Pipeline.succeed RequiredQueryParams
        |> Pipeline.required (Query.enum "instrument"
            (Dict.fromList
                [ ( "piano", Piano )
                , ( "guitar", Guitar )
                , ( "flute", Flute )
                ]
            ))
        |> Pipeline.required (Query.int "bpm")
        |> Pipeline.optional (Query.enum "drums"
            (Dict.fromList
                [ ( "acoustic", Acoustic )
                , ( "electronic", Electronic )
                ]
            ))
        |> Pipeline.optional (Query.string "name")

-- Example with list parameters (multiple same-named params)
type alias ListQueryParams =
    { posts : List String
    , tags : List String
    , ids : List Int
    , mainInstrument : Maybe Instrument
    }

listQueryParser : Query.Parser (Maybe ListQueryParams)
listQueryParser =
    Pipeline.succeed ListQueryParams
        |> Pipeline.with (Query.custom "post[]" identity)
        |> Pipeline.with (Query.custom "tag" identity)
        |> Pipeline.with (Query.custom "id" (List.filterMap String.toInt))
        |> Pipeline.optional (Query.enum "main"
            (Dict.fromList
                [ ( "piano", Piano )
                , ( "guitar", Guitar )
                , ( "flute", Flute )
                ]
            ))

-- Helper function to parse from query string
parseQueryString : String -> Maybe QueryParams
parseQueryString queryString =
    case Url.fromString ("http://example.com" ++ queryString) of
        Just url ->
            Parser.parse (Parser.top <?> queryParser) url
                |> Maybe.withDefault Nothing
        Nothing ->
            Nothing

-- Helper function for required parser
parseRequiredQueryString : String -> Maybe RequiredQueryParams
parseRequiredQueryString queryString =
    case Url.fromString ("http://example.com" ++ queryString) of
        Just url ->
            Parser.parse (Parser.top <?> requiredQueryParser) url
                |> Maybe.withDefault Nothing
        Nothing ->
            Nothing

-- Helper function for list parser
parseListQueryString : String -> Maybe ListQueryParams
parseListQueryString queryString =
    case Url.fromString ("http://example.com" ++ queryString) of
        Just url ->
            Parser.parse (Parser.top <?> listQueryParser) url
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

        listExampleQueries =
            [ "?post[]=foo&post[]=bar&tag=music&tag=rock&id=1&id=2&main=piano"
            , "?post[]=hello&post[]=world&tag=test"
            , "?id=10&id=20&id=30&main=guitar"
            , "?post[]=single&main=flute"
            , "?post[1]=foo&post[2]=bar&tag=indexed"
            ]

        parseExample query =
            let
                optionalParsed = parseQueryString query
                requiredParsed = parseRequiredQueryString query
            in
            div [ style "margin" "10px", style "padding" "10px", style "border" "1px solid #ccc" ]
                [ div [] [ text ("Query: " ++ query) ]
                , div [] [ text ("Optional Parser: " ++ Debug.toString optionalParsed) ]
                , div [] [ text ("Required Parser: " ++ Debug.toString requiredParsed) ]
                ]

        parseListExample query =
            let
                listParsed = parseListQueryString query
            in
            div [ style "margin" "10px", style "padding" "10px", style "border" "1px solid #orange" ]
                [ div [] [ text ("Query: " ++ query) ]
                , div [] [ text ("List Parser: " ++ Debug.toString listParsed) ]
                ]
    in
    div [ style "padding" "20px", style "font-family" "monospace" ]
        [ div [ style "font-size" "18px", style "font-weight" "bold", style "margin-bottom" "20px" ]
            [ text "elm-url-query-pipeline Demo" ]
        , div [] (List.map parseExample exampleQueries)
        , div [ style "margin-top" "30px", style "font-size" "16px", style "font-weight" "bold" ]
            [ text "List Parameters Examples:" ]
        , div [] (List.map parseListExample listExampleQueries)
        , div [ style "margin-top" "20px", style "background" "#f0f0f0", style "padding" "15px" ]
            [ text "Usage Patterns:"
            , div [ style "margin" "10px 0" ]
                [ text "Optional Parameters:"
                , pre [ style "margin" "5px 0", style "font-size" "12px" ]
                    [ text """Pipeline.succeed RecordConstructor
    |> Pipeline.optional (Query.enum "param" (Dict.fromList [...]))
    |> Pipeline.optional (Query.int "number")""" ]
                ]
            , div [ style "margin" "10px 0" ]
                [ text "Required Parameters:"
                , pre [ style "margin" "5px 0", style "font-size" "12px" ]
                    [ text """Pipeline.succeed RecordConstructor
    |> Pipeline.required (Query.enum "param" (Dict.fromList [...]))
    |> Pipeline.required (Query.int "number")""" ]
                ]
            , div [ style "margin" "10px 0" ]
                [ text "List Parameters (multiple same names):"
                , pre [ style "margin" "5px 0", style "font-size" "12px" ]
                    [ text """Pipeline.succeed RecordConstructor
    |> Pipeline.with (Query.custom "post[]" identity)
    |> Pipeline.with (Query.custom "tag" identity)
    |> Pipeline.with (Query.custom "id" (List.filterMap String.toInt))""" ]
                ]
            ]
        ]
