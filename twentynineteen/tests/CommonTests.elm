module CommonTests exposing (suite)

import Array
import Common exposing (losslessMap)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Common tests "
        [ describe "losslessMap tests"
            [ test "Keeps all Justs" <|
                \_ ->
                    Expect.equal
                        (Just [ 1, 2, 3, 4 ])
                        (losslessMap Just [ 1, 2, 3, 4 ])
            , test "Lose all when one fails" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (losslessMap
                            (\x ->
                                if x > 0 then
                                    Just x

                                else
                                    Nothing
                            )
                            [ 1, 2, 3, -4, 5, 6 ]
                        )
            ]
        ]
