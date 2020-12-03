module View exposing (dayView, page)

import Html exposing (Html, div, text)
import Html.Attributes as Attributes


dayView :
    { input : model
    , part1 : model -> String
    , part1Expected : Maybe String
    , part2 : model -> String
    , part2Expected : Maybe String
    }
    -> List (Html msg)
dayView day =
    [ div [] [ text "Part 1" ] ]
        ++ partView day.input day.part1 day.part1Expected
        ++ [ div [] [ text "Part 2" ] ]
        ++ partView day.input day.part2 day.part2Expected


partView : model -> (model -> String) -> Maybe String -> List (Html msg)
partView model solve expected =
    let
        answer =
            solve model
    in
    expected
        |> Maybe.map (\e -> checkExpected e answer)
        |> Maybe.withDefault answer
        |> (\str -> [ text str ])
        |> div []
        |> List.singleton


checkExpected : String -> String -> String
checkExpected expected actual =
    if expected /= actual then
        "Expected " ++ expected ++ " but got " ++ actual

    else
        actual


page : List (Html msg) -> Html msg
page content =
    div [] (css "/style.css" :: content)


css path =
    Html.node "link" [ Attributes.rel "stylesheet", Attributes.href path ] []
