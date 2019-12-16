module Day5 exposing (..)

import Array exposing (Array)
import Inputs.Day5
import Intcode


inputArr =
    Inputs.Day5.inputString |> String.split "," |> List.filterMap String.toInt |> Array.fromList


part1 =
    Intcode.runFromArray [ 1 ] inputArr


part2 =
    Intcode.runFromArray [ 5 ] inputArr
