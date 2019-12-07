module Day2 exposing (..)

import Array exposing (Array)
import Inputs.Day2 exposing (inputString)


intcodeToArr : String -> Array Int
intcodeToArr s =
    Array.fromList <|
        (String.trim s
            |> String.split ","
            |> List.filterMap String.toInt
        )


runProgram : Array Int -> Array Int
runProgram ic =
    runHelper 0 ic


runHelper : Int -> Array Int -> Array Int
runHelper pos ic =
    case Array.get pos ic of
        Just 99 ->
            ic

        Nothing ->
            ic

        Just opCode ->
            let
                mx =
                    getOnceRemoved (pos + 1) ic

                my =
                    getOnceRemoved (pos + 2) ic

                mposres =
                    Array.get (pos + 3) ic
            in
            case ( mx, my, mposres ) of
                ( Just x, Just y, Just posRes ) ->
                    let
                        res =
                            case opCode of
                                1 ->
                                    x + y

                                2 ->
                                    x * y

                                _ ->
                                    0
                    in
                    runHelper
                        (pos + 4)
                        (Array.set posRes res ic)

                _ ->
                    ic



-- This function is for getting an element of an array by retrieving the
-- position also from the array


getOnceRemoved : Int -> Array Int -> Maybe Int
getOnceRemoved pos arr =
    Array.get pos arr
        |> Maybe.andThen (\pos2 -> Array.get pos2 arr)


part1 : Int
part1 =
    Maybe.withDefault 0 <|
        Array.get 0 <|
            runProgram
                (intcodeToArr inputString
                    |> Array.set 1 12
                    |> Array.set 2 2
                )


intLimit =
    99


findNounVerb : Int -> Array Int -> Maybe ( Int, Int )
findNounVerb goal ic =
    findNVHelper 0 0 goal ic


findNVHelper : Int -> Int -> Int -> Array Int -> Maybe ( Int, Int )
findNVHelper n v goal ic =
    let
        run =
            runProgram (ic |> Array.set 1 n |> Array.set 2 v)
    in
    if reachedGoal goal run then
        Just ( n, v )

    else if v == intLimit then
        if n == intLimit then
            Nothing

        else
            findNVHelper (n + 1) 0 goal ic

    else
        findNVHelper n (v + 1) goal ic


reachedGoal : Int -> Array Int -> Bool
reachedGoal goal ic =
    case Array.get 0 ic of
        Nothing ->
            False

        Just num ->
            num == goal


part2 : Int
part2 =
    findNounVerb 19690720 (intcodeToArr inputString)
        |> Maybe.map (\( n, v ) -> 100 * n + v)
        |> Maybe.withDefault 0
