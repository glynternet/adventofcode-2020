module Day8 exposing (main)

import AdventOfCode
import List.Extra
import Result.Extra
import Set


main =
    AdventOfCode.day
        { dayNumber = 8
        , input = inputReal
        , testInput = inputTest
        , part1 = part1
        , part1TestExpected = Just "5"
        , part1Expected = Just "2025"
        , part2 = part2
        , part2Expected = Just "2001"
        , debugWindows = \_ -> []
        }


type alias Model =
    { index : Int
    , accum : Int
    }


part1 : List String -> String
part1 =
    parseInstructions
        >> Result.map (runDevice Nothing Set.empty (Model 0 0))
        >> Result.Extra.join
        >> Result.map
            (\seqEnd ->
                case seqEnd of
                    InfiniteLoop accumilatorValueBeforeLoop ->
                        String.fromInt accumilatorValueBeforeLoop

                    _ ->
                        "Part 1 should reach an InfiniteLoop, not this."
            )
        >> Result.Extra.merge


part2 : List String -> String
part2 input =
    case input |> parseInstructions of
        Err errStr ->
            errStr

        Ok insts ->
            part2Loop insts 0


part2Loop : List ( String, Int ) -> Int -> String
part2Loop original adjustIndex =
    case adjustInstructions adjustIndex original of
        Err str ->
            str

        Ok adjustedInstructions ->
            runDevice Nothing Set.empty (Model 0 0) adjustedInstructions
                |> Result.map
                    (\seqEnd ->
                        case seqEnd of
                            InfiniteLoop _ ->
                                part2Loop original (adjustIndex + 1)

                            ExitedSuccessfully accum ->
                                String.fromInt accum
                    )
                >> Result.Extra.merge


adjustInstructions : Int -> List ( String, Int ) -> Result String (List ( String, Int ))
adjustInstructions index =
    List.Extra.splitAt index
        >> (\( left, right ) ->
                case right of
                    [] ->
                        Ok left

                    el0 :: others ->
                        replaceInst el0 |> Result.map (\replaced -> List.append left (replaced :: others))
           )


replaceInst : ( String, Int ) -> Result String ( String, Int )
replaceInst inst =
    case Tuple.first inst of
        --probably need to keep this index for when switching with jmp?
        "nop" ->
            Ok ( "jmp", Tuple.second inst )

        "acc" ->
            Ok inst

        "jmp" ->
            Ok ( "nop", Tuple.second inst )

        other ->
            Err <| "Could not find instruction for " ++ other ++ "when swapping"


type SequenceEnd
    = InfiniteLoop Int
    | ExitedSuccessfully Int


runDevice : Maybe Model -> Set.Set Int -> Model -> List ( String, Int ) -> Result String SequenceEnd
runDevice previousModel seenIndices currentModel instructions =
    if Set.member currentModel.index seenIndices then
        previousModel |> Maybe.map (.accum >> InfiniteLoop >> Ok) |> Maybe.withDefault (Err "Somehow ended on no previous index")

    else if currentModel.index == List.length instructions then
        Ok <| ExitedSuccessfully currentModel.accum

    else
        case doInstruction currentModel instructions of
            Err err ->
                Err err

            Ok newModel ->
                runDevice (Just currentModel) (Set.insert currentModel.index seenIndices) newModel instructions


doInstruction : Model -> List ( String, Int ) -> Result String Model
doInstruction model instructions =
    getInstruction model.index instructions
        |> Result.map (\i -> i model)


getInstruction : Int -> List ( String, Int ) -> Result String (Model -> Model)
getInstruction index instrs =
    List.Extra.getAt index instrs
        |> Result.fromMaybe ("No instruction at index: " ++ String.fromInt index)
        |> Result.map instr
        |> (\outerRes ->
                case outerRes of
                    Err str ->
                        Err str

                    Ok innerRes ->
                        innerRes
           )


instr : ( String, Int ) -> Result String (Model -> Model)
instr i =
    case Tuple.first i of
        "nop" ->
            Ok (\m -> { m | index = m.index + 1 })

        "acc" ->
            Ok (\m -> { m | index = m.index + 1, accum = m.accum + Tuple.second i })

        "jmp" ->
            Ok (\m -> { m | index = m.index + Tuple.second i })

        other ->
            Err <| "Could not find instruction for " ++ other


parseInstructions : List String -> Result String (List ( String, Int ))
parseInstructions =
    List.map parseInstruction
        >> Result.Extra.combine


parseInstruction : String -> Result String ( String, Int )
parseInstruction str =
    case String.split " " str of
        [ op, num ] ->
            String.toInt num
                |> Result.fromMaybe "Err parsing number from instruction"
                |> Result.map (\i -> ( op, i ))

        _ ->
            Err "Expected two parts to instruction"


inputTest : List String
inputTest =
    String.split "\n"
        """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""


inputReal : List String
inputReal =
    String.split "\n" """acc +50
acc -11
nop +378
acc +15
jmp +508
acc -3
jmp +1
jmp +135
jmp +1
acc -6
acc +14
acc +32
jmp +315
acc -16
jmp +249
jmp +283
acc -14
acc +5
acc +29
jmp +366
acc +22
jmp +77
acc +19
jmp +496
acc -2
acc -16
nop +284
nop +36
jmp +178
jmp +281
acc +32
acc +45
acc +16
jmp +403
nop +86
nop +32
acc +10
jmp +47
acc -13
acc +35
jmp +270
jmp +1
acc +34
acc -3
nop +116
jmp +552
acc +27
nop +113
jmp +495
acc -18
acc +47
acc +19
jmp +180
acc -8
acc -1
acc -14
acc +17
jmp +431
acc +49
acc +22
acc +39
acc +28
jmp +74
jmp -10
acc -5
acc +35
jmp +251
acc +31
acc -11
jmp -49
acc -12
acc +49
jmp +36
acc -19
acc -9
acc +11
acc -1
jmp +419
jmp +307
acc +36
jmp +563
acc +32
acc +1
jmp +270
acc +17
jmp +464
jmp +133
acc +29
acc +31
jmp +394
acc -2
jmp +94
acc +44
acc +28
acc +32
jmp +543
acc +18
jmp +325
acc +16
acc +42
jmp +315
acc -6
jmp +371
acc +41
acc +29
jmp +44
acc -19
jmp +393
acc +4
jmp +81
acc +25
jmp +108
acc -18
jmp +1
jmp +1
acc +34
jmp +124
acc +25
acc +45
jmp -46
acc -11
acc +43
acc +50
jmp +6
acc +3
acc -6
acc +38
acc +9
jmp +402
acc +26
nop +97
acc +26
jmp +115
acc -1
acc +2
jmp +7
acc +38
nop +5
jmp -75
acc +41
nop +470
jmp +15
acc -15
acc +19
acc +22
jmp +240
acc +14
acc +26
jmp +71
acc +38
acc +25
jmp +349
acc +25
acc +31
acc +41
jmp +419
jmp -69
acc +50
nop +218
jmp -106
nop +225
jmp +307
acc +33
acc -4
acc +36
jmp -57
acc +14
acc +0
acc -2
jmp +184
acc +47
nop +161
acc -4
jmp -149
jmp +103
acc +39
acc +25
acc +8
acc +2
jmp +364
acc +48
jmp +241
nop +432
acc +9
jmp +304
acc +20
jmp +223
acc +12
acc +21
jmp +121
acc +12
acc +47
acc +50
acc +8
jmp +283
jmp +1
jmp +81
acc +22
acc -6
jmp +1
acc -9
jmp +340
acc -9
acc +5
acc +11
jmp +204
acc -13
acc +12
jmp +322
acc +38
acc +50
nop +211
jmp +91
acc +31
acc +34
jmp -95
acc +12
acc +13
jmp -172
nop +419
jmp +1
nop -191
acc +48
jmp +157
acc +22
acc +27
jmp +61
acc +23
nop +181
jmp -121
nop +367
jmp -168
jmp +1
nop -218
jmp -142
jmp +295
jmp +112
acc +9
acc -12
jmp +114
acc +50
jmp -28
acc +18
nop -223
acc +37
acc -14
jmp +169
acc +0
acc +42
jmp +115
acc +2
acc +31
jmp -189
acc +7
acc +45
acc -2
acc +34
jmp -121
acc -13
acc +4
nop -94
acc +34
jmp +123
acc -11
acc -13
jmp -29
acc -11
nop -169
acc -11
nop +369
jmp +189
acc -4
jmp +20
nop +19
acc -13
nop +368
jmp -79
acc -19
acc +23
acc -7
acc -11
jmp +36
acc -18
acc +31
nop +349
acc +11
jmp -106
acc +43
jmp +185
acc +20
nop +297
jmp +138
acc +8
acc +26
acc -2
jmp -18
nop -276
jmp +44
jmp +1
acc +39
jmp +314
acc +0
jmp -194
acc +32
acc +17
acc +43
jmp -298
acc +28
acc -10
jmp -103
acc -17
acc +3
jmp +25
acc +35
acc +7
acc -2
jmp -39
acc +19
acc +19
acc -8
jmp -282
jmp -275
acc -7
jmp +196
acc +14
acc +5
jmp +6
acc -7
jmp +29
nop +275
acc -12
jmp +165
acc +21
acc +4
jmp +95
acc +15
jmp -283
jmp +199
acc -9
acc +0
jmp -220
acc +28
acc +1
jmp -313
acc +13
acc -5
acc +38
jmp +62
acc +43
jmp -159
acc -14
acc +44
jmp -314
acc +3
acc +34
jmp +47
jmp -171
acc +27
acc +11
acc +16
jmp +16
acc +27
acc +40
jmp +66
acc +30
acc -15
jmp +177
acc +36
acc +41
jmp -189
acc -19
jmp +106
nop +271
nop -176
acc +13
jmp +40
nop +33
jmp -324
acc +18
jmp -76
acc +38
acc +39
acc +34
jmp +231
jmp -131
acc +46
acc +38
acc -3
jmp -161
acc +31
acc +10
jmp +158
acc -18
acc +46
jmp -291
jmp +48
acc +18
acc +36
acc +16
jmp -77
acc +9
jmp -289
acc +38
jmp -388
nop +137
acc +42
acc +17
nop -37
jmp -145
jmp -336
acc +46
acc -18
acc -13
acc +21
jmp -97
acc +49
nop -189
acc +21
jmp -186
acc +25
acc +37
jmp +193
jmp +1
acc -14
acc +4
jmp +87
acc +3
nop -95
jmp -243
acc +30
acc +35
jmp -128
jmp +1
nop +55
acc +48
jmp +129
jmp +1
acc +37
jmp -326
acc -2
acc -13
acc +37
jmp -72
acc +23
jmp +130
acc +18
acc +0
acc +36
jmp -345
acc +0
acc +23
acc +10
jmp +1
jmp -112
nop -430
acc +8
acc +42
jmp +1
jmp +180
nop -16
acc +22
jmp +1
acc +2
jmp +43
acc +29
acc +23
acc -2
jmp -364
acc +14
jmp -250
acc -11
nop -359
jmp +132
jmp -24
nop +90
acc +32
jmp -461
jmp -311
acc +11
acc +21
jmp -320
jmp -194
jmp -165
acc +43
acc +5
acc +12
jmp -419
jmp -467
acc +47
acc +35
jmp +133
acc +10
nop -394
acc +35
nop -109
jmp -298
acc -10
nop -451
jmp -445
jmp +57
acc +31
nop -1
jmp -59
acc +19
acc +7
jmp -5
acc +31
acc +0
acc +29
acc -8
jmp -118
jmp -119
acc +35
jmp -339
acc +14
nop +28
acc +0
acc +25
jmp -265
acc -9
acc +29
jmp -365
nop +19
acc +31
acc +16
jmp -116
jmp -442
acc +24
acc -3
jmp -505
acc -5
jmp -485
acc -12
acc +15
jmp +1
jmp -16
acc +23
nop -135
jmp +26
acc -16
jmp -374
jmp -171
jmp -518
acc +23
acc +23
jmp -282
nop -78
nop -230
jmp -285
acc +39
acc +31
jmp -219
acc -18
jmp +1
acc +43
jmp -175
acc +46
nop -391
jmp -305
acc -11
acc +41
acc +33
acc -9
jmp +70
nop -8
acc -3
acc -16
acc +8
jmp -139
nop -237
acc +1
nop -405
acc +16
jmp +14
acc +0
acc +35
acc +26
acc +43
jmp +71
nop -187
nop -188
jmp -7
acc +34
acc +11
nop -35
jmp -104
jmp -37
jmp +1
acc +37
acc +1
nop -78
jmp +19
acc +35
acc +35
acc -3
acc +0
jmp -377
acc +49
jmp -519
acc -18
acc -5
acc -15
nop -76
jmp -530
acc +7
acc +0
jmp -19
acc +15
acc +37
jmp -79
jmp -339
nop -398
acc -16
jmp +20
acc -15
acc -5
acc +20
acc -12
jmp -21
acc +39
acc +32
acc +34
jmp -330
acc +48
acc +2
acc -8
acc -15
jmp -231
acc +35
acc -16
acc +26
nop -547
jmp -548
acc +6
acc +20
acc +1
jmp -439
jmp -310
acc +7
acc +18
jmp -58
nop -444
jmp -423
acc -5
jmp -40
acc -14
acc -11
nop -283
jmp -122
acc +13
acc +5
nop -259
acc +12
jmp +1"""
