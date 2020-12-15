module Day15 exposing (main)

import AdventOfCode
import Dict
import List.Extra
import MultiDict
import Result.Extra


main =
    AdventOfCode.day
        { dayNumber = 15
        , input = inputReal
        , testInput = inputTest
        , processInput = processInput
        , part1 = part1
        , part1TestExpected = Just "436"
        , part1Expected = Just "410"
        , part2 = part2
        , part2TestExpected = Just "175594"
        , part2Expected = Just "238"
        , debugWindows = \_ -> []
        }


type alias InputModel =
    List Int


type alias TurnModel =
    { turnNumber : Int
    , spoken : Int
    , numberToTurnsAfterSpeaking : Dict.Dict Int (List Int)
    }


part1 : InputModel -> String
part1 input =
    setup input
        |> nextLoop 2019
        |> Result.map (.spoken >> String.fromInt)
        |> Result.Extra.merge


nextLoop : Int -> Result String TurnModel -> Result String TurnModel
nextLoop maxIters lastRes =
    case lastRes of
        Err err ->
            Err err

        Ok last ->
            if last.turnNumber > maxIters then
                lastRes

            else
                nextLoop maxIters (next last)


debugTurnModel : TurnModel -> String
debugTurnModel tm =
    String.fromInt tm.turnNumber ++ ": " ++ String.fromInt tm.spoken ++ ": " ++ Debug.toString tm.numberToTurnsAfterSpeaking


next : TurnModel -> Result String TurnModel
next turnModel =
    calculateNextNumberToSpeak turnModel
        |> Result.map
            (\nextNumberToSpeak ->
                let
                    nextTurnNumber =
                        turnModel.turnNumber + 1

                    turnsAlreadyInDict =
                        Dict.get nextNumberToSpeak turnModel.numberToTurnsAfterSpeaking
                            |> Maybe.withDefault []

                    nextTurnDictEntry =
                        List.append
                            (List.reverse turnsAlreadyInDict |> List.take 1)
                            [ nextTurnNumber ]
                in
                TurnModel
                    nextTurnNumber
                    nextNumberToSpeak
                    (Dict.insert
                        nextNumberToSpeak
                        nextTurnDictEntry
                        turnModel.numberToTurnsAfterSpeaking
                    )
            )


setup : InputModel -> Result String TurnModel
setup input =
    List.Extra.last input
        |> Result.fromMaybe "No last spoken number"
        |> Result.map
            (\last ->
                TurnModel
                    (List.length input)
                    last
                    (input
                        |> List.indexedMap (\i val -> ( val, i + 1 ))
                        >> MultiDict.fromEntryList
                    )
            )


calculateNextNumberToSpeak : TurnModel -> Result String Int
calculateNextNumberToSpeak lastTurnModel =
    case Dict.get lastTurnModel.spoken lastTurnModel.numberToTurnsAfterSpeaking of
        Nothing ->
            Ok 0

        Just previousTurnNumbers ->
            case previousTurnNumbers |> List.reverse |> List.take 2 of
                [ max, secondMax ] ->
                    Ok <| max - secondMax

                _ ->
                    Ok 0


part2 : InputModel -> String
part2 input =
    setup input
        |> nextLoop 29999999
        |> Result.map (.spoken >> String.fromInt)
        |> Result.Extra.merge


processInput : String -> Result String InputModel
processInput =
    String.split "," >> List.map (String.toInt >> Result.fromMaybe "not int") >> Result.Extra.combine


inputTest : String
inputTest =
    """0,3,6"""


inputReal : String
inputReal =
    """7,12,1,0,16,2"""
