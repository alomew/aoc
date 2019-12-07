module Common exposing (..)

import Dict exposing (Dict)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


losslessMap : (a -> Maybe b) -> List a -> Maybe (List b)
losslessMap f l =
    List.foldl
        (\param acc ->
            let
                mItem =
                    f param
            in
            case acc of
                Nothing ->
                    Nothing

                Just sf ->
                    case mItem of
                        Just item ->
                            Just (item :: sf)

                        Nothing ->
                            Nothing
        )
        (Just [])
        l
        |> Maybe.map List.reverse


type alias Point2D =
    ( Int, Int )


manhattanDistance : Point2D -> Point2D -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


weakDictInsert : comparable -> a -> Dict comparable a -> Dict comparable a
weakDictInsert k v d =
    if Dict.member k d then
        d

    else
        Dict.insert k v d
