module Day4 exposing (..)

import Common
import List.Extra as List


input =
    "134792-675810"


comparingAdjacents : (a -> a -> Bool) -> List a -> List Bool
comparingAdjacents comp list =
    case list of
        [] ->
            []

        [ _ ] ->
            []

        _ :: xs ->
            List.map (\( u, v ) -> comp u v) <| List.zip list xs


nonDecreasing : List Char -> Bool
nonDecreasing chars =
    List.all identity <| comparingAdjacents (<=) chars


adjacentDuplicate : List Char -> Bool
adjacentDuplicate chars =
    List.any identity <| comparingAdjacents (==) chars


validPassword : List Char -> Bool
validPassword chars =
    List.length chars
        == 6
        && nonDecreasing chars
        && adjacentDuplicate chars


part1 : Int
part1 =
    List.length <|
        List.filter validPassword <|
            List.map (String.fromInt >> String.toList) <|
                List.range 134792 675810


validPassword2 : List Char -> Bool
validPassword2 chars =
    List.foldl
        (\char s ->
            case s of
                Nothing ->
                    Nothing

                Just state ->
                    case state.prev of
                        Nothing ->
                            Just { state | prev = Just char, count = 1 }

                        Just c ->
                            if c == char then
                                Just { state | count = state.count + 1 }

                            else if c > char then
                                Nothing

                            else if state.count == 2 then
                                Just { state | hasDup = True, prev = Just char, count = 1 }

                            else
                                Just { state | prev = Just char, count = 1 }
        )
        (Just { prev = Nothing, count = 0, hasDup = False })
        chars
        |> Maybe.map
            (\state ->
                if state.count == 2 then
                    { state | hasDup = True }

                else
                    state
            )
        |> Maybe.andThen
            (\state ->
                if state.hasDup then
                    Just True

                else
                    Nothing
            )
        |> Common.isJust


validPassword3 : List Char -> Bool
validPassword3 chars =
    List.sort chars
        == chars
        && (List.any (\( _, others ) -> List.length others + 1 == 2) <| List.group chars)


part2 : Int
part2 =
    List.length <|
        List.filter validPassword3 <|
            List.map (String.fromInt >> String.toList) <|
                List.range 134792 675810
