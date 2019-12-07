module Day4Tests exposing (suite)

import Array
import Day4 exposing (part2)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Day 4 tests "
        [ test "Part 2" <|
            \_ ->
                Expect.equal
                    1319
                    part2
        ]
