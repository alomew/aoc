module Main exposing (..)

import Browser
import Day1
import Day2
import Html exposing (div, h1, h2, text)


init =
    [ ( Day1.part1, Day1.part2 )
    , ( Day2.part1, Day2.part2 )
    ]


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    List ( Int, Int )


update _ model =
    model


viewDay day ( p1, p2 ) =
    div []
        [ h2 [] [ text <| "Day " ++ String.fromInt day ]
        , div [] [ text ("Part 1: " ++ String.fromInt p1) ]
        , div [] [ text ("Part 2: " ++ String.fromInt p2) ]
        ]


view model =
    div []
        ([ h1 [] [ text "Advent of Code, 2019" ] ]
            ++ List.indexedMap (\i parts -> viewDay (i + 1) parts) model
        )
