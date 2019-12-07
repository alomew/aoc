module Day2Tests exposing (..)

import Array
import Day2 exposing (findNounVerb, intcodeToArr, runProgram)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Day2"
        [ describe "Intcodes can be converted to arrays"
            [ test "quicky" <|
                \_ ->
                    Expect.equal
                        [ 1, 9, 10, 3 ]
                        (Array.toList (intcodeToArr "1,9,10,3"))
            ]
        , describe "Intcode programs run properly"
            [ test "main example" <|
                \_ ->
                    Expect.equal
                        [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
                        (runProgram (intcodeToArr "1,9,10,3,2,3,11,0,99,30,40,50") |> Array.toList)
            ]
        , describe "Noun and verb search"
            [ test "based on main example" <|
                \_ ->
                    Expect.equal
                        (Just ( 9, 10 ))
                        (findNounVerb 3500 (intcodeToArr "1,6,85,3,2,3,11,0,99,30,40,50"))
            ]
        ]
