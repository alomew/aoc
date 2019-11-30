module Prep exposing (..)

import Parser as Prs exposing ((|.), (|=), Parser, int, run, spaces, succeed, symbol)
import Result exposing (Result)
import String exposing (lines)

{-
    2018 Challenge 1

    Input looks like +1, -2, +3, +1

-}

signedInt : Parser Int
signedInt =
    Prs.oneOf
    [ Prs.succeed negate
        |. Prs.symbol "-"
        |= Prs.int
    , Prs.succeed identity
        |. Prs.symbol "+"
        |= Prs.int
    ]

frequencies : Parser (List Int)
frequencies =
    Prs.loop [] frequenciesHelp

frequenciesHelp : List Int -> Parser (Prs.Step (List Int) (List Int))
frequenciesHelp revFreqs =
    Prs.oneOf
    [ Prs.succeed (\freq -> Prs.Loop (freq :: revFreqs))
        |= signedInt
        |. Prs.oneOf
        [Prs.symbol ","
            |. Prs.spaces
        , Prs.succeed ()]
    , Prs.succeed ()
        |> Prs.map (\_ -> Prs.Done (List.reverse revFreqs))
    ]

tester1 () =
    Prs.run frequencies "+1, -2, +3, +1"


{-
    Challenge 2
    Input looks like
    abcdef
    bababc
    abbcde
    abcccd
-}

ids : String -> List String
ids = lines


{-
    Challenge 3
    Input looks like
    #1 @ 1,3: 4x4
    #2 @ 3,1: 4x4
    #3 @ 5,5: 2x2
-}

-- we write a parser for one of those lines
type alias Claim = {id: Int, x : Int, y : Int, width : Int, height : Int}

claim : Parser Claim
claim =
    succeed Claim
    |. symbol "#"
    |= int
    |. symbol " @ "
    |= int
    |. symbol ","
    |= int
    |. symbol ":"
    |. spaces
    |= int
    |. symbol "x"
    |= int

tester3 =
    run claim "#2 @ 3,1: 4x4"