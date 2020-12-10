module Day10 exposing (main)

import AdventOfCode
import Result.Extra


main =
    AdventOfCode.day
        { dayNumber = 10
        , input = processInputString inputReal
        , testInput = processInputString inputTest
        , part1 = part1
        , part1TestExpected = Nothing
        , part1Expected = Nothing
        , part2 = part2
        , part2Expected = Nothing
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
                --|> Result.map (\( last, count1, count3 ) -> String.fromInt count1 ++ "\t" ++ String.fromInt count3)
                |> Result.Extra.merge



--|> List.length
--|> String.fromInt
--part1Loop : List Int
--part1Loop list =
--    case list of
--        [] -> 0


part2 : Result String (List Int) -> String
part2 input =
    ""


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
