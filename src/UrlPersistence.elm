module UrlPersistence exposing
    ( QueryData
    , load
    , reset
    , serialize
    )

import Grid exposing (PercGrid, PitchGrid)
import Instruments exposing (DrumKit, TonalInstrument)
import Scales exposing (RootNote, ScaleType)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Url.Query.Pipeline as Pipeline


type alias QueryData a =
    { a
        | bpm : Int
        , octaveStart : Int
        , octaveCount : Int
        , pitchGrid : PitchGrid
        , percGrid : PercGrid
        , scaleType : ScaleType
        , rootNote : RootNote
        , bars : Int
        , beatsPerBar : Int
        , subdivisions : Int
        , currentTonalInstrument : TonalInstrument
        , currentDrumKit : DrumKit
    }


type alias QueryParams =
    { bpm : Maybe Int
    , octaveStart : Maybe Int
    , octaveCount : Maybe Int
    , pitchGrid : Maybe PitchGrid
    , percGrid : Maybe PercGrid
    , scaleType : Maybe ScaleType
    , rootNote : Maybe RootNote
    , bars : Maybe Int
    , beatsPerBar : Maybe Int
    , subdivisions : Maybe Int
    , currentTonalInstrument : Maybe TonalInstrument
    , currentDrumKit : Maybe DrumKit
    }


queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Pipeline.succeed QueryParams
        |> Pipeline.optional (Query.int "bpm")
        |> Pipeline.optional (Query.int "octaveStart")
        |> Pipeline.optional (Query.int "octaveCount")
        |> Pipeline.optional (Query.string "pitchGrid" |> Query.map (Maybe.andThen Grid.parsePitchGrid))
        |> Pipeline.optional (Query.string "percGrid" |> Query.map (Maybe.andThen Grid.parsePercGrid))
        |> Pipeline.optional (Query.string "scale" |> Query.map (Maybe.map Scales.parseScaleType))
        |> Pipeline.optional (Query.string "root" |> Query.map (Maybe.map Scales.parseRootNote))
        |> Pipeline.optional (Query.int "bars")
        |> Pipeline.optional (Query.int "beatsPerBar")
        |> Pipeline.optional (Query.int "subdivisions")
        |> Pipeline.optional (Query.string "instrument" |> Query.map (Maybe.map Instruments.parseTonal))
        |> Pipeline.optional (Query.string "drumKit" |> Query.map (Maybe.map Instruments.parseDrumKit))


parseQueryParams : Url -> Maybe QueryParams
parseQueryParams url =
    if url.query == Nothing then
        Nothing

    else
        Parser.parse (Parser.top <?> queryParser) url
            |> Maybe.andThen identity


buildQueryString : QueryData a -> String
buildQueryString data =
    UB.absolute
        []
        [ UB.int "bpm" data.bpm
        , UB.int "octaveStart" data.octaveStart
        , UB.int "octaveCount" data.octaveCount
        , UB.string "pitchGrid" (Grid.pitchGridToString data.pitchGrid)
        , UB.string "percGrid" (Grid.percGridToString data.percGrid)
        , UB.string "scale" (Scales.scaleLabel data.scaleType)
        , UB.string "root" (Scales.rootNoteToString data.rootNote)
        , UB.int "bars" data.bars
        , UB.int "beatsPerBar" data.beatsPerBar
        , UB.int "subdivisions" data.subdivisions
        , UB.string "instrument" (Instruments.tonalLabel data.currentTonalInstrument)
        , UB.string "drumKit" (Instruments.drumKitLabel data.currentDrumKit)
        ]


buildQueryStringFromUrl : Url -> String
buildQueryStringFromUrl url =
    url.query |> Maybe.map (\q -> "/?" ++ q) |> Maybe.withDefault "/"


load : Url -> QueryData a -> QueryData a
load url data =
    case parseQueryParams url of
        Just params ->
            { data
                | bpm = Maybe.withDefault data.bpm params.bpm
                , octaveStart = Maybe.withDefault data.octaveStart params.octaveStart
                , octaveCount = Maybe.withDefault data.octaveCount params.octaveCount
                , pitchGrid = Maybe.withDefault data.pitchGrid params.pitchGrid
                , percGrid = Maybe.withDefault data.percGrid params.percGrid
                , scaleType = Maybe.withDefault data.scaleType params.scaleType
                , rootNote = Maybe.withDefault data.rootNote params.rootNote
                , bars = Maybe.withDefault data.bars params.bars
                , beatsPerBar = Maybe.withDefault data.beatsPerBar params.beatsPerBar
                , subdivisions = Maybe.withDefault data.subdivisions params.subdivisions
                , currentTonalInstrument = Maybe.withDefault data.currentTonalInstrument params.currentTonalInstrument
                , currentDrumKit = Maybe.withDefault data.currentDrumKit params.currentDrumKit
            }

        Nothing ->
            reset data


reset : QueryData a -> QueryData a
reset data =
    { data
        | pitchGrid = Grid.emptyPitchGrid
        , percGrid = Grid.emptyPercGrid
        , scaleType = Scales.Major
        , rootNote = Scales.C
        , octaveStart = 3
        , octaveCount = 3
        , bars = 8
        , beatsPerBar = 4
        , subdivisions = 2
        , bpm = 120
        , currentTonalInstrument = Instruments.defaultTonalInstrument
        , currentDrumKit = Instruments.defaultDrumKit
    }


serialize : Url -> QueryData a -> Maybe String
serialize url data =
    let
        dataQuery =
            buildQueryString data

        urlQuery =
            buildQueryStringFromUrl url
    in
    if urlQuery == dataQuery then
        Nothing

    else
        Just dataQuery
