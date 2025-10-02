module Grid exposing
    ( PercGrid
    , PercPos
    , PitchGrid
    , PitchPos
    , convertMelodyToGrid
    , convertPercussionToGrid
    , emptyPercGrid
    , emptyPitchGrid
    , isPercCellActive
    , isPitchCellActive
    , parsePercGrid
    , parsePitchGrid
    , percGridToString
    , pitchGridToString
    , resizePitchGrid
    , shiftStepRight
    , transposePitchGrid
    , updatePercCell
    , updatePitchCell
    )

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Set exposing (Set)
import Timing exposing (TimeConfig)
import Utils exposing (times)



-- Grid Types


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )



-- Basic Grid Operations


emptyPitchGrid : PitchGrid
emptyPitchGrid =
    Set.empty


emptyPercGrid : PercGrid
emptyPercGrid =
    Set.empty


isPitchCellActive : PitchPos -> PitchGrid -> Bool
isPitchCellActive { pitchIdx, stepIdx } pitchGrid =
    Set.member ( pitchIdx, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchIdx, stepIdx ) pitchGrid

    else
        Set.remove ( pitchIdx, stepIdx ) pitchGrid


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    Set.member (percPositionToTuple position) grid


updatePercCell : PercPos -> Bool -> PercGrid -> PercGrid
updatePercCell position isActive grid =
    let
        tuple =
            percPositionToTuple position
    in
    if isActive then
        Set.insert tuple grid

    else
        Set.remove tuple grid


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Grid Transformations


resizePitchGrid : ScaleConfig -> ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid oldConfig newConfig newTimeConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    midiPitch =
                        Scales.pitchIdxToMidi pitchIdx oldConfig
                in
                case Scales.midiToPitchIdx midiPitch newConfig of
                    Just newPitchIdx ->
                        if newPitchIdx < Scales.getTotalPitches newConfig && stepIdx < Timing.getTotalSteps newTimeConfig then
                            Just ( newPitchIdx, stepIdx )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


transposePitchGrid : ScaleConfig -> ScaleConfig -> PitchGrid -> PitchGrid
transposePitchGrid oldConfig newConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    scaleDegreeInfo =
                        Scales.pitchIdxToScaleDegree pitchIdx oldConfig
                in
                case Scales.scaleDegreeToPitchIdx scaleDegreeInfo newConfig of
                    Just newPitchIdx ->
                        Just ( newPitchIdx, stepIdx )

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


shiftStepRight : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
shiftStepRight fromStepIdx totalSteps pitchGrid percGrid =
    let
        newPitchGrid =
            pitchGrid
                |> Set.toList
                |> List.filterMap
                    (\( pitchIdx, stepIdx ) ->
                        if stepIdx >= fromStepIdx then
                            let
                                newStepIdx =
                                    stepIdx + 1
                            in
                            if newStepIdx < totalSteps then
                                Just ( pitchIdx, newStepIdx )

                            else
                                Nothing

                        else
                            Just ( pitchIdx, stepIdx )
                    )
                |> Set.fromList

        newPercGrid =
            percGrid
                |> Set.toList
                |> List.filterMap
                    (\( percRowIdx, stepIdx ) ->
                        if stepIdx >= fromStepIdx then
                            let
                                newStepIdx =
                                    stepIdx + 1
                            in
                            if newStepIdx < totalSteps then
                                Just ( percRowIdx, newStepIdx )

                            else
                                Nothing

                        else
                            Just ( percRowIdx, stepIdx )
                    )
                |> Set.fromList
    in
    ( newPitchGrid, newPercGrid )



-- Grid Conversions


convertMelodyToGrid : List (List String) -> ScaleConfig -> PitchGrid
convertMelodyToGrid stepMelodies config =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        case Scales.noteNameToPitchIdx noteName config of
                            Just pitchIdx ->
                                Just ( pitchIdx, stepIdx )

                            Nothing ->
                                Nothing
                    )
                    noteNames
            )
        |> List.concat
        |> Set.fromList


convertPercussionToGrid : List (List PercType) -> PercGrid
convertPercussionToGrid stepPercussion =
    stepPercussion
        |> List.indexedMap
            (\stepIdx percTypes ->
                List.map
                    (\percType ->
                        ( Instruments.percRowIdx percType, stepIdx )
                    )
                    percTypes
            )
        |> List.concat
        |> Set.fromList



-- Grid Serialization


{-| Convert PitchGrid to comma-separated integers: "0,0,1,2,5,10"
-}
pitchGridToString : PitchGrid -> String
pitchGridToString grid =
    grid
        |> Set.toList
        |> List.concatMap (\( pitch, step ) -> [ String.fromInt pitch, String.fromInt step ])
        |> String.join ","


{-| Parse comma-separated integers back to PitchGrid: "0,0,1,2,5,10"
-}
parsePitchGrid : String -> Maybe PitchGrid
parsePitchGrid str =
    let
        _ =
            Debug.log "str" str
    in
    if String.isEmpty str then
        Just Set.empty

    else
        str
            |> String.split ","
            |> List.filterMap String.toInt
            |> pairUp
            |> Maybe.map Set.fromList


{-| Convert PercGrid to comma-separated integers: "0,0,1,2,5,10"
-}
percGridToString : PercGrid -> String
percGridToString grid =
    grid
        |> Set.toList
        |> List.concatMap (\( perc, step ) -> [ String.fromInt perc, String.fromInt step ])
        |> String.join ","


{-| Parse comma-separated integers back to PercGrid: "0,0,1,2,5,10"
-}
parsePercGrid : String -> Maybe PercGrid
parsePercGrid str =
    if String.isEmpty str then
        Just Set.empty

    else
        str
            |> String.split ","
            |> List.filterMap String.toInt
            |> pairUp
            |> Maybe.map Set.fromList


{-| Group list into pairs: [0,0,1,2] -> [(0,0),(1,2)]
-}
pairUp : List Int -> Maybe (List ( Int, Int ))
pairUp list =
    case list of
        [] ->
            Just []

        x :: y :: rest ->
            pairUp rest
                |> Maybe.map (\pairs -> ( x, y ) :: pairs)

        _ ->
            Nothing
