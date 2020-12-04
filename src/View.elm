module View exposing (dayView, page)

import Html exposing (Html, div, text)
import Html.Attributes as Attributes


dayView :
    { input : model
    , testInput : model
    , part1 : model -> String
    , part1TestExpected : Maybe String
    , part1Expected : Maybe String
    , part2 : model -> String
    , part2Expected : Maybe String
    }
    -> List (Html msg)
dayView day =
    [ partView "Test 1" day.testInput day.part1 day.part1TestExpected
    , partView "Part 1" day.input day.part1 day.part1Expected
    , partView "Part 2" day.input day.part2 day.part2Expected
    ]


partView : String -> model -> (model -> String) -> Maybe String -> Html msg
partView name model solve maybeExpected =
    let
        answer =
            solve model
    in
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


page : List (Html msg) -> Html msg
page content =
    div []
        (css "/style.css"
            :: content
        )


css path =
    Html.node "link" [ Attributes.rel "stylesheet", Attributes.href path ] []
