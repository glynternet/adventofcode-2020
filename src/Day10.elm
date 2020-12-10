module Day10 exposing (main)

import AdventOfCode
import Dict
import Result.Extra


main =
    AdventOfCode.day
        { dayNumber = 10
        , input = processInputString inputReal
        , testInput = processInputString inputTest
        , part1 = part1
        , part1TestExpected = Just "35"
        , part1Expected = Just "2210"
        , part2 = part2Res
        , part2TestExpected = Just "8"
        , part2Expected = Just "7086739046912"
        , debugWindows = \_ -> []
        }


part1 : Result String (List Int) -> String
part1 input =
    case input of
        Err err ->
            err

        Ok ints ->
            ints
                |> List.sort
                |> List.foldl
                    (\new accum ->
                        case accum of
                            Err str ->
                                Err str

                            Ok ( last, count1, count3 ) ->
                                case new - last of
                                    1 ->
                                        Ok ( new, count1 + 1, count3 )

                                    3 ->
                                        Ok ( new, count1, count3 + 1 )

                                    2 ->
                                        Ok ( new, count1, count3 )

                                    other ->
                                        Err <| ("Difference was " ++ String.fromInt other)
                    )
                    -- always a 3 volt different at end for my device
                    (Ok ( 0, 0, 1 ))
                |> Result.map (\( last, count1, count3 ) -> String.fromInt (count1 * count3))
                |> Result.Extra.merge


part2Res : Result String (List Int) -> String
part2Res input =
    case input of
        Err err ->
            err

        Ok ints ->
            part2 ints


part2 : List Int -> String
part2 input =
    case List.maximum input of
        Nothing ->
            "No max"

        Just max ->
            let
                voltageList =
                    List.sort input ++ [ max + 3 ]
            in
            part2Loop (Debug.log "startDict" (Dict.insert 0 1 Dict.empty))
                (Debug.log "list " voltageList)
                |> Result.map String.fromInt
                |> Result.Extra.merge


part2Loop : Dict.Dict Int Int -> List Int -> Result String Int
part2Loop currentPaths joltages =
    let
        _ =
            Debug.log "currentPaths" currentPaths
    in
    case joltages of
        [] ->
            Dict.keys currentPaths
                |> List.sort
                |> List.maximum
                |> Result.fromMaybe "No max?!"
                |> Result.map (\max -> Dict.get max currentPaths |> Result.fromMaybe "Max not in paths")
                |> Result.Extra.join

        lowest :: others ->
            [ 1, 2, 3 ]
                |> List.map (\sub -> lowest - sub)
                |> List.map (\key -> Debug.log (String.fromInt key) (Dict.get key currentPaths) |> Maybe.withDefault 0)
                |> List.sum
                |> (\sum -> part2Loop (Dict.insert lowest sum currentPaths) others)


listsStartingBelowOrAt : Int -> List Int -> List (List Int)
listsStartingBelowOrAt n list =
    case list of
        [] ->
            []

        a :: others ->
            if a < n then
                [ a :: others ] ++ listsStartingBelowOrAt n others

            else
                []


processInputString : List String -> Result String (List Int)
processInputString =
    List.map (String.toInt >> Result.fromMaybe "Couldn't convert to int") >> Result.Extra.combine


inputTest : List String
inputTest =
    String.split "\n"
        """16
10
15
5
1
11
7
19
6
12
4"""


inputReal : List String
inputReal =
    String.split "\n" """126
38
162
123
137
97
92
67
136
37
146
2
139
74
101
163
128
127
13
111
30
117
3
93
29
152
80
21
7
54
69
40
48
104
110
142
57
116
31
70
28
151
108
20
157
121
47
75
94
39
73
77
129
41
24
44
132
87
114
58
64
4
10
19
138
45
76
147
59
155
156
83
118
109
107
160
61
91
102
115
68
150
34
16
27
135
161
46
122
90
1
164
100
103
84
145
51
60"""
