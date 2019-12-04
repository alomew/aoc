module Day3Tests exposing (suite)

import Array
import Day3 exposing (Direction(..), WireStrand, crossoverPoints, parseWire, shortestCombinedSteps)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Day 3 tests "
        [ describe
            "Check parser"
            [ test "Parse short example" <|
                \_ ->
                    Expect.equal
                        (Just [ WireStrand Right 8, WireStrand Up 5, WireStrand Left 5, WireStrand Down 3 ])
                        (parseWire "R8,U5,L5,D3")
            ]
        , describe
            "Crossover system tests"
            [ test "Example given" <|
                \_ ->
                    let
                        crossovers =
                            Maybe.map2 crossoverPoints
                                (parseWire "R8,U5,L5,D3")
                                (parseWire "U7,R6,D4,L4")
                    in
                    case crossovers of
                        Just crossings ->
                            Expect.equalSets
                                (Set.fromList [ ( 3, 3 ), ( 6, 5 ) ])
                                crossings

                        Nothing ->
                            Expect.fail "Couldn't parse the wires"
            ]
        , describe
            "Shortest joint steps"
            [ test "Example given" <|
                \_ ->
                    Expect.equal
                        (Just <| Just 30)
                        (Maybe.map2 shortestCombinedSteps (parseWire "R8,U5,L5,D3") (parseWire "U7,R6,D4,L4"))
            ]
        ]
