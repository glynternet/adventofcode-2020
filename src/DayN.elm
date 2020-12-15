module DayN exposing (main)

import AdventOfCode


main =
    AdventOfCode.day
        { dayNumber = 0
        , input = inputReal
        , testInput = inputTest
        , processInput = processInput
        , part1 = part1
        , part1TestExpected = Nothing
        , part1Expected = Nothing
        , part2 = part2
        , part2TestExpected = Nothing
        , part2Expected = Nothing
        , debugWindows = \_ -> []
        }


part1 : List String -> String
part1 input =
    ""


part2 : List String -> String
part2 input =
    ""


processInput : String -> Result String (List String)
processInput =
    String.lines >> Ok


inputTest : String
inputTest =
    """"""


inputReal : String
inputReal =
    """"""
