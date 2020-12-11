module Day11 exposing (main)

import AdventOfCode
import Array
import Grid
import Result.Extra


main =
    AdventOfCode.day
        { dayNumber = 11
        , input = inputReal
        , testInput = inputTest
        , part1 = part1
        , part1TestExpected = Just """37"""
        , part1Expected = Just "2194"
        , part2 = part2
        , part2TestExpected = Just "26"
        , part2Expected = Just "1944"
        , debugWindows =
            \_ ->
                [ ( Just "Debugging"
                  , case inputTest |> mapDict of
                        Err err ->
                            err

                        Ok map ->
                            List.range 1 3
                                |> List.foldl
                                    (\i currList ->
                                        case currList of
                                            head :: others ->
                                                ( i, head |> Tuple.second |> part2iterateSeatmap |> Tuple.second ) :: currList

                                            _ ->
                                                currList
                                    )
                                    [ ( 0, map ) ]
                                |> List.reverse
                                |> List.map (\( i, m ) -> String.fromInt i ++ "\n" ++ printMap m)
                                |> String.join "\n\n"
                  )
                , ( Just "In View"
                  , case inputDebug |> mapDict of
                        Err err ->
                            err

                        Ok map ->
                            case inViewTileDirection 0 0 ( 1, 0 ) map of
                                Just a ->
                                    tileString a

                                Nothing ->
                                    "Nope"
                  )
                ]
        }


inputDebug : List (List Char)
inputDebug =
    String.split "\n"
        """##
##"""
        |> List.map String.toList


type alias Map =
    Grid.Grid TileState


part1 : List (List Char) -> String
part1 input =
    case input |> mapDict of
        Err err ->
            err

        Ok mapD ->
            mapD |> part1Loop |> Grid.values |> List.filter ((==) Occupied) |> List.length |> String.fromInt


part1Loop : Map -> Map
part1Loop inMap =
    inMap
        |> part1iterateSeatmap
        |> (\( changes, newMap ) ->
                if changes > 0 then
                    part1Loop newMap

                else
                    inMap
           )


part2 : List (List Char) -> String
part2 input =
    case input |> mapDict of
        Err err ->
            err

        Ok mapD ->
            mapD |> part2Loop |> Grid.values |> List.filter ((==) Occupied) |> List.length |> String.fromInt


part2Loop : Map -> Map
part2Loop inMap =
    inMap
        |> (\map ->
                let
                    iterated =
                        part2iterateSeatmap map

                    _ =
                        Debug.log "map" (Tuple.second iterated |> printMap)
                in
                iterated
           )
        |> (\( changes, newMap ) ->
                if changes > 0 then
                    part2Loop newMap

                else
                    inMap
           )


part1iterateSeatmap : Map -> ( Int, Map )
part1iterateSeatmap mapNow =
    let
        newMap =
            mapNow
                |> Grid.map
                    (\x y tile ->
                        let
                            states =
                                adjacentTileStates x y mapNow
                        in
                        case tile of
                            Empty ->
                                if
                                    (states |> occupiedCount)
                                        == 0
                                then
                                    ( 1, Occupied )

                                else
                                    ( 0, Empty )

                            Occupied ->
                                if (states |> occupiedCount) >= 4 then
                                    ( 1, Empty )

                                else
                                    ( 0, Occupied )

                            other ->
                                ( 0, other )
                    )
    in
    ( newMap |> Grid.map (\_ _ tuple -> Tuple.first tuple) |> Grid.values |> List.sum
    , newMap |> Grid.map (\_ _ tuple -> Tuple.second tuple)
    )


occupiedCount : List TileState -> Int
occupiedCount =
    List.foldl
        (\state count ->
            if state == Occupied then
                count + 1

            else
                count
        )
        0


part2iterateSeatmap : Map -> ( Int, Map )
part2iterateSeatmap mapNow =
    let
        newMap =
            mapNow
                |> Grid.map
                    (\x y tile ->
                        let
                            states =
                                inViewTiles x y mapNow
                        in
                        case tile of
                            Empty ->
                                if (states |> occupiedCount) == 0 then
                                    ( 1, Occupied )

                                else
                                    ( 0, Empty )

                            Occupied ->
                                if (states |> occupiedCount) >= 5 then
                                    ( 1, Empty )

                                else
                                    ( 0, Occupied )

                            other ->
                                ( 0, other )
                    )
    in
    ( newMap |> Grid.map (\_ _ tuple -> Tuple.first tuple) |> Grid.values |> List.sum
    , newMap |> Grid.map (\_ _ tuple -> Tuple.second tuple)
    )


inViewTiles : Int -> Int -> Map -> List TileState
inViewTiles x y map =
    directions
        |> List.filterMap (\dir -> inViewTileDirection x y dir map)


inViewTileDirection : Int -> Int -> ( Int, Int ) -> Map -> Maybe TileState
inViewTileDirection x y dir map =
    let
        state =
            Grid.get (x + Tuple.first dir) (y + Tuple.second dir) map
    in
    case state of
        Nothing ->
            Nothing

        Just tileState ->
            case tileState of
                Floor ->
                    inViewTileDirection (x + Tuple.first dir) (y + Tuple.second dir) dir map

                seat ->
                    Just seat


directions : List ( Int, Int )
directions =
    List.range -1 1
        |> List.map
            (\x ->
                List.range -1 1
                    |> List.map
                        (\y ->
                            if x == 0 && y == 0 then
                                Nothing

                            else
                                Just ( x, y )
                        )
            )
        |> List.concat
        |> List.filterMap identity


adjacentTileStates : Int -> Int -> Map -> List TileState
adjacentTileStates currentX currentY map =
    directions
        |> List.map (\dir -> ( currentX + Tuple.first dir, currentY + Tuple.second dir ))
        |> List.filterMap
            (\( xx, yy ) -> Grid.get xx yy map)


printMap : Grid.Grid TileState -> String
printMap =
    Array.map
        (\row ->
            row |> rowString
        )
        >> Array.toList
        >> String.join "\n"


rowString : Array.Array TileState -> String
rowString =
    Array.map tileString
        >> Array.toList
        >> String.join ""


tileString : TileState -> String
tileString tile =
    case tile of
        Empty ->
            "L"

        Floor ->
            "."

        Occupied ->
            "#"


type TileState
    = Floor
    | Occupied
    | Empty


mapDict : List (List Char) -> Result String Map
mapDict input =
    input
        |> Grid.mapFromLists charToTileState
        |> (\g ->
                if g |> Grid.values |> List.any Result.Extra.isErr then
                    Err "There are some map errors"

                else
                    Ok <| Grid.map (\_ _ res -> res |> Result.withDefault Floor) g
           )


charToTileState char =
    case char of
        'L' ->
            Ok Empty

        '#' ->
            Ok Occupied

        '.' ->
            Ok Floor

        other ->
            Err ("Unexpected character: " ++ String.fromChar other)


combineArrayRes : Array.Array (Result b c) -> Result b (Array.Array c)
combineArrayRes dict =
    Array.foldl resultFoldHelper (Ok Array.empty) dict


resultFoldHelper : Result b c -> Result b (Array.Array c) -> Result b (Array.Array c)
resultFoldHelper res accum =
    case accum of
        Err err ->
            Err err

        Ok array ->
            case res of
                Err err ->
                    Err err

                Ok value ->
                    Ok <| Array.append array (Array.fromList [ value ])


inputTest : List (List Char)
inputTest =
    String.split "\n"
        """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
        |> List.map String.toList


inputReal : List (List Char)
inputReal =
    String.split "\n" """LLLLLLL.LLLLLLLLLLLL.LL.L.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
LLLLLLL.LLLLLLLLLLLL.LLL..LLLLLLLLLLLLLLLLLLLLLLL..LLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLL.LLLL
LLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLL.L.LLLLLLLL.L.LLLLLL.LLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
.L.....L...L.....LL..L...LLL.L.LL..LLL..LL.LLL...LLLL..L......L..........L...L..LL..LLL.L...L
LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLL.LLLLL.L.LLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLL
LLLL.LL.LLLLLL.LLLL.LLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLL.LLLLLLLLL.LLLL.
LLLLLLL.LLLLLL.LLLLL..LLL.LLLLLL.L.LLLLLLLLL.LLLL..LLLLLLL.L.L.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLL
LLL.LLL.LLLLLLLLLL.L.LLLL..LLLLLLLLLL.LL.LL.LLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLL.L.LLLLL.LLLLLLL.LL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.
LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
L...L..LLL.....L.LL..L.LLLL....LLLL.....L.L.LLL..L.L...LL.LL....L..LLLL..L..L.LL...L.L.....L.
LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLL.L.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLL.LLLLLLL.LLLLLLLLLL.LLLLL
LLLLLLL.LLLLLLLLLLLL.LLLL.LLLL.LLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.L.LL.LLLLL.LLLLLLLLLLLLL
LLLLLLL..LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL
.........LL..LL..LL..LL.....L..L..LL.............L...L....LLLL...LL...LLL..L...LLL.....L....L
LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL..LLLLLL.L.LLL
LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
LLLLLLL.LL.LLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLL
LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LL..LLLLLLLLLLLL
LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLLLLLL.L.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL
...L.L..L.L.L....L....L.LLL.L.L.L..L..L...L....L......L.......L..L.L.L..L..LL...L....L....LL.
LLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.L.LL.LL.LL.LLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLL.L.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL..LLL..LLLL.LLLL.LL.LLLLL
LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLL.LLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL..LLLL
LLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
..LLLLL...LL...L..L.....L.LL.L....L.L..LL.L......L.L.L..L...L.L..L..L..L.LL..L.L.L..L.....L..
LLLLLLL.LLLLLL.LLL.L.LLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLL.LL.LLLLL.LLLL.LLLLLLLL.LLLLLL.LL.LLLL..LLLLLL.LLLLLLLLLL.LLLL..LLLL.LLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
.LLLLLL.LLLLLL.LLLLL.LLLLLLLLL.LLL.LLLLLLL.L.LLLLLLLLLLLLLLLL.LLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
LLLLLLL.LLL.LL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLL.LL.LL.LLLLLLLLLLLLLL.LL.LLLL.LL.LLLL.
LLLLLLL.LLLLLL.LLLLLLLLLL.L..LLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
LL......LLL...L.LL...LL..LLLL.L..L.........L..L.....L..L......L..L...LLL.....LL.......LL.....
LLLLLLL.LLLLLL.LLLLL.L.LL.LLLLLLLL.LLLL.LLLLL.L.LLLLLLLLLL.LLLLLLLLL.LLLLLLLLL..LLLLLLLLLLLLL
LLLLLLL.L.LLLL.LLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LL.LLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL..LLLLLLLLLLLLLLLLLLLLLLL.L.LLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.L.LLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLL.LLLLLLLLLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLL.L.LLLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
L....L.L..L.L....L..L.L...L.....L..LL...L.L.L.L.LL..L....LL..LL....L.L.......LLL..LLL...LL..L
LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLL..LLLLLLLLL.LLLL..LLLL.LLLLLLLLLLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLL..LLLLL.LLLLLLLLLLLLL.LLLLLL.LL..LLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLL.LLL
L.L.LLLLLLLLLL.LLLLL.LLLL.LLLLL.LL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLL.LLL.LLLL.LLLLLLLLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.L.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLL.LLLL.LLLLLLLLLLLLLLLLLL.
LL.L.LLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLL
...L..L..L..L....LLL..L.LL....L........L.......L.L.LL.L........L...LLLL....LL.......L.LL..L.L
LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL
LLLLLLL.LLLLLL.LL.LL.LLLLLLLLLL.LL.LLLLLLLLL.LLLLL.LLL..LLLLLLLLLLLL.LLLL..LL.L.LLLLLLL.LLLLL
LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLL.L.LL.LL.LLLLLLL.LLLLLLLLLL.LLL.LLLLL.LLLLLLL.LLLLL
LLLLLLLLLLL.LL.L.LLL.LLLL.LLLLLLL..LLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLL.LLLL..LLLLLLLLLLLL.LLLLL
LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.L.LLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.L.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.L.LLL.LLLLLLL..LLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.L.LL.L.LLL.LLLLLLL.LLLL.
....LLLL.L..L....LL..L.....LL..........LL.....L.L..LL.LL......L.L.L.L....L.LL.L.L..L......L..
LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLL
LLLLLLLLLLL.LLLLLLLL.LLLL.LLLLLLLL.LLL.LLLLL.LLLL..LLLLLLLLLLLLLLLLL.LLLL.LLLLL..LLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLLLLLLLLL.LL.LLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LL.LL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLL.
LLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL..LLLLLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLL..LLLL
LLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LL.LLLLLLL.LLLLLLL.L.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLLLLLLLLL.LLLLL.LLLL.LLLLLLLL.LLLL.LLLL.LLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLL
LLLLLL..LLLLLLLLLL.L.LLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLLLL.LLLLLLL.L.LLL
LLLLLLLLLL.LLLLLLLLL.LL.L.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLL.LLL.LLLLL
L.L...L....LL.L....LL..L...L...LL.L.............LLLLL..LL........LLL.L....L.L...L.L...LLL.LL.
LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLLL.LLLLLL.LLLLL
LL.LLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.L.LLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLLLL.LLLLLLLLLLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLL.LLL.LLLLLLLLLLL.LLL
.LLL..LL.L.LL.L...L.L..L...L....L.L................L....L.......L..L.....L...LLL....L....LL..
LLLLL.LLLLLLLL.LLLLL.L.LL.LLLLLLLL.LLLLLLLLL..LLLLLLLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLLL
LLLLLLL.LLLLLL.LLLLLLLLLL.LL.L.LLL.LLLLLL.LL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLL.LLLLLLLLL.LLLLL
LLLLLLL.LLLLLL.LLLLL.LLLL.LLLLLLLLLL.L.LLLLL.LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLL
LLLLLL..LLLLLLLLLLLLLLLLL.LLLLLLLL.L.LLL.LLL.LLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLL
LLLLLLL.LLLL.L.LLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLL.LLLL.LLLLL.LLLL.LLLLLLLL
LLLLLLL.LLLLLLLLLLLL..LLL.LLLLLLLL..LLLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLL.LLLLL""" |> List.map String.toList
