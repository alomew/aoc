module Main exposing (..)

import Browser
import Day1
import Day2
import Day3
import Html exposing (Html, div, h1, h2, text)


init =
    [ ( Just Day1.part1, Just Day1.part2 )
    , ( Just Day2.part1, Just Day2.part2 )
    , ( Nothing, Day3.part2 )
    ]


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    List ( Maybe Int, Maybe Int )


update _ model =
    model


textOfPart : Maybe Int -> String
textOfPart mi =
    Maybe.map String.fromInt mi |> Maybe.withDefault "That went very wrong."


viewDay day ( p1, p2 ) =
    div []
        [ h2 [] [ text <| "Day " ++ String.fromInt day ]
        , div [] [ text ("Part 1: " ++ textOfPart p1) ]
        , div [] [ text ("Part 2: " ++ textOfPart p2) ]
        ]


view : Model -> Html msg
view model =
    div []
        ([ h1 [] [ text "Advent of Code, 2019" ] ]
            ++ List.indexedMap (\i parts -> viewDay (i + 1) parts) model
        )
