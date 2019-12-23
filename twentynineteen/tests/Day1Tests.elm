module Day1Tests exposing (..)

import Day1 exposing (fuel, part1, part2, totalFuel)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Day 1"
        [ describe "module fuel requirements"
            [ test "get right fuel for 14" <|
                \_ -> Expect.equal 2 (fuel 14)
            , test "calculate fuel for mass 1969" <|
                \_ -> Expect.equal 654 (fuel 1969)
            ]
        , test "part 1" <|
            \_ -> Expect.equal 3216868 part1
        , describe "recursive fuel requirements"
            [ test "correct for module 14" <|
                \_ -> Expect.equal 2 (totalFuel 14)
            , test "correct for module 1969" <|
                \_ -> Expect.equal 966 (totalFuel 1969)
            ]
        , test "part 2" <|
            \_ -> Expect.equal 4822435 part2
        ]
