module AdventOfCode exposing (day)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes as Attributes


day : Day model -> Program () (Day model) msg
day impl =
    Browser.sandbox
        { init = impl
        , view = view
        , update = \_ model -> model
        }


type alias Day model =
    { dayNumber : Int
    , input : String
    , testInput : String
    , processInput : String -> Result String model
    , part1 : model -> String
    , part1TestExpected : Maybe String
    , part1Expected : Maybe String
    , part2 : model -> String
    , part2Expected : Maybe String
    , part2TestExpected : Maybe String
    , debugWindows : model -> List ( Maybe String, String )
    }


view : Day model -> Html msg
view dayModel =
    div [ Attributes.class "page" ]
        (css "/style.css"
            :: div [] [ dayModel.dayNumber |> String.fromInt |> (++) "Day " |> text ]
            :: (case dayModel.processInput dayModel.testInput of
                    Err err ->
                        [ div [] [ text <| "Error processing test input: " ++ err ] ]

                    Ok testModel ->
                        case dayModel.processInput dayModel.input of
                            Err err ->
                                [ div [] [ text <| "Error processing real input: " ++ err ] ]

                            Ok realModel ->
                                partViews ( testModel, realModel ) dayModel
                                    ++ (dayModel.debugWindows testModel
                                            |> List.map
                                                (\( title, str ) ->
                                                    div []
                                                        ((title
                                                            |> Maybe.map (\t -> [ text t, Html.br [] [] ])
                                                            |> Maybe.withDefault []
                                                         )
                                                            ++ [ text <| str ]
                                                        )
                                                )
                                       )
               )
        )


partViews : ( model, model ) -> Day model -> List (Html msg)
partViews models =
    cases models
        >> List.foldl
            (\case_ ( lastPassed, elements ) ->
                if not lastPassed then
                    ( False, elements )

                else
                    let
                        answer =
                            case_.solution case_.model
                    in
                    ( pass answer case_.expected, partView case_.name answer case_.expected :: elements )
            )
            ( True, [] )
        >> Tuple.mapSecond List.reverse
        >> Tuple.second


type alias Case model =
    { name : String
    , solution : model -> String
    , model : model
    , expected : Maybe String
    }


cases : ( model, model ) -> Day model -> List (Case model)
cases ( testModel, realModel ) dayModel =
    [ Case "Test 1" dayModel.part1 testModel dayModel.part1TestExpected
    , Case "Part 1" dayModel.part1 realModel dayModel.part1Expected
    , Case "Test 2" dayModel.part2 testModel dayModel.part2TestExpected
    , Case "Part 2" dayModel.part2 realModel dayModel.part2Expected
    ]


pass : String -> Maybe String -> Bool
pass answer =
    Maybe.map ((==) answer)
        >> Maybe.withDefault False


partView : String -> String -> Maybe String -> Html msg
partView name answer maybeExpected =
    maybeExpected
        |> Maybe.map (\e -> checkExpected e answer)
        |> Maybe.withDefault answer
        |> (\str ->
                div
                    [ Attributes.class
                        (maybeExpected
                            |> Maybe.map
                                (\expected ->
                                    if expected == answer then
                                        "gold"

                                    else
                                        "red"
                                )
                            |> Maybe.withDefault "grey"
                        )
                    ]
                    [ text name
                    , Html.br [] []
                    , text str
                    ]
           )


checkExpected : String -> String -> String
checkExpected expected actual =
    if expected /= actual then
        "Expected " ++ expected ++ " but got " ++ actual

    else
        actual


css path =
    Html.node "link" [ Attributes.rel "stylesheet", Attributes.href path ] []
