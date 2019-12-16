module Day6 exposing (..)

import Inputs.Day6 exposing (input)
import Parser as P exposing ((|.), (|=), Parser)
import Set exposing (Set)


type alias OrbitRelation =
    ( String, String )


parseOrbitRelation : Parser OrbitRelation
parseOrbitRelation =
    P.succeed Tuple.pair
        |= (P.chompWhile Char.isAlphaNum |> P.getChompedString)
        |. P.symbol ")"
        |= (P.chompWhile Char.isAlphaNum |> P.getChompedString)


type System
    = Object String (List System)


insertIntoSystems : OrbitRelation -> List System -> List System -> Maybe (List System)
insertIntoSystems (( centre, orbitter ) as or) options checked =
    case options of
        [] ->
            Nothing

        sys :: otherSys ->
            case sys of
                Object id children ->
                    if id == centre then
                        Just <| Object id (Object orbitter [] :: children) :: (otherSys ++ checked)

                    else
                        case insertIntoSystems or children [] of
                            Just newChildren ->
                                Just <| Object id newChildren :: (otherSys ++ checked)

                            Nothing ->
                                insertIntoSystems or otherSys (sys :: checked)


insertIntoSystem : OrbitRelation -> System -> Maybe System
insertIntoSystem or sys =
    case insertIntoSystems or [ sys ] [] of
        Just [ newSys ] ->
            Just newSys

        _ ->
            Nothing


setUpSystem : List OrbitRelation -> List OrbitRelation -> System -> System
setUpSystem ors tryAgain sys =
    case ( ors, tryAgain ) of
        ( [], [] ) ->
            sys

        ( [], _ ) ->
            setUpSystem tryAgain [] sys

        ( or :: restOrs, _ ) ->
            case insertIntoSystem or sys of
                Just newSys ->
                    setUpSystem restOrs tryAgain newSys

                Nothing ->
                    setUpSystem restOrs (or :: tryAgain) sys


inputSystem : System
inputSystem =
    let
        orbitRelations =
            List.filterMap (\orString -> Result.toMaybe <| P.run parseOrbitRelation orString) input
    in
    setUpSystem orbitRelations [] (Object "COM" [])


countOrbits : System -> Int
countOrbits so =
    countOrbitsHelper 1 so


countOrbitsHelper : Int -> System -> Int
countOrbitsHelper depth so =
    case so of
        Object _ [] ->
            0

        Object _ xs ->
            (List.sum <| List.map (countOrbitsHelper (depth + 1)) xs) + List.length xs * depth


part1 =
    inputSystem |> countOrbits


pathTo : System -> String -> Set String -> Maybe (Set String)
pathTo sys id sofar =
    case sys of
        Object objId children ->
            if objId == id then
                Just sofar

            else
                case children of
                    [] ->
                        Nothing

                    _ ->
                        case List.filterMap (\i -> pathTo i id Set.empty) children of
                            path :: _ ->
                                Just <| Set.insert objId (Set.union path sofar)

                            _ ->
                                Nothing


part2 =
    let
        pathToYou =
            Maybe.withDefault Set.empty (pathTo inputSystem "YOU" Set.empty)

        pathToSanta =
            Maybe.withDefault Set.empty (pathTo inputSystem "SAN" Set.empty)
    in
    Set.size <| Set.union (Set.diff pathToYou pathToSanta) (Set.diff pathToSanta pathToYou)


example =
    Object "COM"
        [ Object
            "B"
            [ Object "G"
                [ Object "H" [] ]
            , Object "C"
                [ Object "D"
                    [ Object "E"
                        [ Object "F" []
                        , Object "J"
                            [ Object "K"
                                [ Object "L" []
                                , Object "YOU" []
                                ]
                            ]
                        ]
                    , Object "I" [ Object "SAN" [] ]
                    ]
                ]
            ]
        ]


exampleOrbitRelations =
    [ ( "COM", "B" )
    , ( "B", "C" )
    , ( "C", "D" )
    , ( "D", "E" )
    , ( "E", "F" )
    , ( "B", "G" )
    , ( "G", "H" )
    , ( "D", "I" )
    , ( "E", "J" )
    , ( "J", "K" )
    , ( "K", "L" )
    ]
