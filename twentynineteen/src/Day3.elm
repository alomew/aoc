module Day3 exposing (..)

import Common exposing (Point2D)
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


pointsOverStrand : Point2D -> WireStrand -> ( Set Point2D, Point2D )
pointsOverStrand ( startX, startY ) { direction, length } =
    let
        orderedPoints =
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
    ( Set.fromList orderedPoints, Maybe.withDefault ( 0, 0 ) <| List.last orderedPoints )


allPoints : Wire -> Set Point2D
allPoints wire =
    List.foldl
        (\strand { currentPlace, points } ->
            let
                ( strandPoints, lastPoint ) =
                    pointsOverStrand currentPlace strand
            in
            { currentPlace = lastPoint
            , points = Set.union strandPoints points
            }
        )
        { currentPlace = ( 0, 0 ), points = Set.empty }
        wire
        |> .points


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
