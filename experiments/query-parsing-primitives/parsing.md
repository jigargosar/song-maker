```elm

type alias A =
    { a1 : Maybe Int
    , a2 : Maybe String
    }


aParser : Query.Parser (Maybe A)
aParser =
    Pipeline.succeed A
        |> Pipeline.optional (Query.int "a1")
        |> Pipeline.optional (Query.string "a2")


type alias B =
    { b1 : Maybe Int
    , b2 : Maybe String
    }


bParser : Query.Parser (Maybe B)
bParser =
    Pipeline.succeed B
        |> Pipeline.optional (Query.int "b1")
        |> Pipeline.optional (Query.string "b2")



-- Top-level record that groups A and B


type alias Top =
    { a : Maybe A
    , b : Maybe B
    }


topParser : Query.Parser (Maybe Top)
topParser =
    Pipeline.succeed Top
        |> Pipeline.optional aParser
        |> Pipeline.optional bParser


parseUrl =
    Url.fromString "https://example.com/"
        |> Maybe.map (Parser.parse (Parser.query topParser))
        |> Maybe.andThen identity
        |> Debug.log "foo"

```