module Day1 exposing (..)

import Inputs.Day1 exposing (inputLines)


fuel : Int -> Int
fuel mass =
    (mass // 3) - 2


totalFuel : Int -> Int
totalFuel moduleMass =
    totalFuelHelper 0 (fuel moduleMass)


totalFuelHelper : Int -> Int -> Int
totalFuelHelper total latest =
    if latest <= 0 then
        total

    else
        totalFuelHelper (total + latest) (fuel latest)


part1 : Int
part1 =
    let
        moduleMasses =
            List.filterMap String.toInt inputLines
    in
    moduleMasses |> List.map fuel |> List.sum


part2 : Int
part2 =
    let
        moduleMasses =
            List.filterMap String.toInt inputLines
    in
    moduleMasses |> List.map totalFuel |> List.sum
