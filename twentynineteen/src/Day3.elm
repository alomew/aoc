module Day3 exposing (..)

import Common exposing (Point2D)
import Dict exposing (Dict)
import Inputs.Day3 exposing (inputStrings)
import List.Extra as List
import Parser as Prs exposing ((|=), Parser)
import Set exposing (Set)


type alias WireStrand =
    { direction : Direction, length : Int }


type Direction
    = Up
    | Down
    | Left
    | Right



-- Parsing input


parsedirection : Parser Direction
parsedirection =
    Prs.oneOf
        [ Prs.map (always Up) (Prs.symbol "U")
        , Prs.map (always Down) (Prs.symbol "D")
        , Prs.map (always Left) (Prs.symbol "L")
        , Prs.map (always Right) (Prs.symbol "R")
        ]


parseWireStrand : Parser WireStrand
parseWireStrand =
    Prs.succeed WireStrand
        |= parsedirection
        |= Prs.int


type alias Wire =
    List WireStrand


parseWire : String -> Maybe Wire
parseWire s =
    s
        |> String.split ","
        |> Common.losslessMap (\wss -> Prs.run parseWireStrand wss |> Result.toMaybe)


pointsOverStrand : Point2D -> WireStrand -> ( List Point2D, Point2D )
pointsOverStrand ( startX, startY ) { direction, length } =
    let
        strandOrderedPoints =
            List.range 1 length
                |> List.map
                    (\i ->
                        case direction of
                            Up ->
                                ( startX, startY + i )

                            Down ->
                                ( startX, startY - i )

                            Left ->
                                ( startX - i, startY )

                            Right ->
                                ( startX + i, startY )
                    )
    in
    ( strandOrderedPoints, Maybe.withDefault ( startX, startY ) <| List.last strandOrderedPoints )


orderedPoints : Wire -> List Point2D
orderedPoints wire =
    List.foldl
        (\strand { currentPlace, points } ->
            let
                ( strandPoints, lastPoint ) =
                    pointsOverStrand currentPlace strand
            in
            { currentPlace = lastPoint
            , points = points ++ strandPoints
            }
        )
        { currentPlace = ( 0, 0 ), points = [] }
        wire
        |> .points


allPoints : Wire -> Set Point2D
allPoints wire =
    Set.fromList <| orderedPoints wire


crossoverPoints : Wire -> Wire -> Set Point2D
crossoverPoints wire1 wire2 =
    Set.intersect (allPoints wire1) (allPoints wire2)


part1 : Maybe Int
part1 =
    case inputStrings of
        [ wire1String, wire2String ] ->
            case Maybe.map2 Tuple.pair (parseWire wire1String) (parseWire wire2String) of
                Nothing ->
                    Nothing

                Just ( wire1, wire2 ) ->
                    crossoverPoints wire1 wire2
                        |> Set.toList
                        |> List.map (Common.manhattanDistance ( 0, 0 ))
                        |> List.minimum

        _ ->
            Nothing


fewestSteps : Wire -> Set Point2D -> Dict Point2D Int
fewestSteps wire wanted =
    List.indexedFoldl
        (\i p d ->
            if Set.member p wanted then
                Common.weakDictInsert p (i + 1) d

            else
                d
        )
        Dict.empty
        (orderedPoints wire)


shortestCombinedSteps : Wire -> Wire -> Maybe Int
shortestCombinedSteps wire1 wire2 =
    let
        crossovers =
            crossoverPoints wire1 wire2

        fewestSteps1 =
            fewestSteps wire1 crossovers

        fewestSteps2 =
            fewestSteps wire2 crossovers
    in
    Common.losslessMap (\point -> Maybe.map2 (+) (Dict.get point fewestSteps1) (Dict.get point fewestSteps2))
        (Set.toList <| crossovers)
        |> Maybe.andThen List.minimum


part2 : Maybe Int
part2 =
    case inputStrings of
        [ wireString1, wireString2 ] ->
            parseWire wireString1
                |> Maybe.andThen
                    (\wire1 ->
                        parseWire wireString2
                            |> Maybe.andThen
                                (\wire2 ->
                                    shortestCombinedSteps wire1 wire2
                                )
                    )

        _ ->
            Nothing
