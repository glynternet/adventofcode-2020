module Grid exposing (..)

import Array
import Result.Extra


type alias Grid a =
    Array.Array (Array.Array a)


fromLists : List (List a) -> Grid a
fromLists =
    mapFromLists identity


mapFromLists : (a -> b) -> List (List a) -> Grid b
mapFromLists mapFunc =
    Array.fromList
        >> Array.map (List.map mapFunc >> Array.fromList)


map : (Int -> Int -> a -> b) -> Grid a -> Grid b
map mapFunc =
    Array.indexedMap
        (\y row ->
            row |> Array.indexedMap (\x v -> mapFunc x y v)
        )


get : Int -> Int -> Grid a -> Maybe a
get x y grid =
    case Array.get y grid of
        Nothing ->
            Nothing

        Just row ->
            Array.get x row


values : Grid a -> List a
values =
    Array.toList >> List.map Array.toList >> List.concat


columns : Grid a -> Result String (List (List a))
columns grid =
    let
        maxColumns =
            Array.map Array.length grid |> Array.toList |> List.maximum |> Maybe.withDefault 0
    in
    List.range 0 (maxColumns - 1)
        |> List.map
            (\colNum ->
                grid
                    |> Array.map (Array.get colNum >> Result.fromMaybe "No column for row")
                    |> Array.toList
                    |> Result.Extra.combine
            )
        |> Result.Extra.combine
