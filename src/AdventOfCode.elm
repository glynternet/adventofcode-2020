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
    { input : model
    , testInput : model
    , part1 : model -> String
    , part1TestExpected : Maybe String
    , part1Expected : Maybe String
    , part2 : model -> String
    , part2Expected : Maybe String
    , debugWindows : model -> List ( Maybe String, String )
    }


view : Day model -> Html msg
view dayModel =
    div []
        (css "/style.css"
            :: [ partView "Test 1" dayModel.testInput dayModel.part1 dayModel.part1TestExpected
               , partView "Part 1" dayModel.input dayModel.part1 dayModel.part1Expected
               , partView "Part 2" dayModel.input dayModel.part2 dayModel.part2Expected
               ]
            ++ (dayModel.debugWindows dayModel.input
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


css path =
    Html.node "link" [ Attributes.rel "stylesheet", Attributes.href path ] []
