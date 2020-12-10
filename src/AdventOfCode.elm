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
    , input : model
    , testInput : model
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
            :: partViews dayModel
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


partViews : Day model -> List (Html msg)
partViews dayModel =
    let
        part1Answer =
            dayModel.part1 dayModel.input

        part1Passed =
            dayModel.part1Expected
                |> Maybe.map (\expected -> expected == part1Answer)
                |> Maybe.withDefault False
    in
    [ partView "Test 1" (dayModel.part1 dayModel.testInput) dayModel.part1TestExpected
    , partView "Part 1" part1Answer dayModel.part1Expected
    ]
        ++ (if part1Passed then
                [ partView "Test 2" (dayModel.part2 dayModel.testInput) dayModel.part2TestExpected
                , partView "Part 2" (dayModel.part2 dayModel.input) dayModel.part2Expected
                ]

            else
                []
           )


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
