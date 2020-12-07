module Day7 exposing (main)

import AdventOfCode
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..), int, oneOf, spaces, succeed, token, variable)
import Result.Extra
import Set


main =
    AdventOfCode.day
        { dayNumber = 7
        , input = inputReal
        , testInput = inputTest
        , part1 = part1
        , part1TestExpected = Nothing
        , part1Expected = Nothing
        , part2 = part2
        , part2Expected = Nothing
        , debugWindows = \_ -> debugWindows
        }


debugWindows : List ( Maybe String, String )
debugWindows =
    [ ( Just "Debugging parseBagCount"
      , Parser.run parseBagCount "1 bright white bags"
            |> Result.mapError (List.map deadEndToString >> String.join "\n")
            |> Result.map bagCountStr
            |> Result.Extra.merge
      )
    , ( Just "Debugging parseContents"
      , Parser.run parseContents "1 bright white bag, 2 muted yellow bags."
            |> Result.mapError (List.map deadEndToString >> String.join "\n")
            |> Result.map (\bagCounts -> List.map bagCountStr bagCounts |> String.join "\t")
            |> Result.Extra.merge
      )
    ]


type alias Rule =
    { root : String
    , somethingElse : String
    }


type alias BagCount =
    ( Int, Bag )


parseRule : Parser ( Bag, List BagCount )
parseRule =
    succeed Tuple.pair
        |= parseBag
        |. spaces
        |. token "bags contain"
        |. spaces
        |= parseContents


parseContents : Parser (List BagCount)
parseContents =
    Parser.loop [] contentsHelp


contentsHelp : List BagCount -> Parser (Parser.Step (List BagCount) (List BagCount))
contentsHelp revStmts =
    --succeed (\stmt -> (Maybe.map (\_ -> Parser.Loop) |> Maybe.withDefault Parser.Done) (maybeToList stmt ++ revStmts))
    oneOf
        [ token ", " |> Parser.map (\_ -> Parser.Loop revStmts)
        , token "." |> Parser.map (\_ -> Parser.Done revStmts)
        , succeed (\stmt -> Parser.Loop (maybeToList stmt ++ revStmts))
            |= parseMaybeBagCount
        ]


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton
        >> Maybe.withDefault []


parseMaybeBagCount : Parser (Maybe BagCount)
parseMaybeBagCount =
    oneOf
        [ Parser.map (\bc -> Just bc) parseBagCount
        , Parser.map (\_ -> Nothing) (token "no other bags")
        ]


parseBagCount : Parser BagCount
parseBagCount =
    succeed Tuple.pair
        |= int
        |. spaces
        |= parseBag
        |. spaces
        |. oneOf
            [ Parser.map (\_ -> ()) (Parser.keyword "bag")
            , Parser.map (\_ -> ()) (Parser.keyword "bags")
            ]


type alias Bag =
    { colourMod : String
    , colour : String
    }


parseBag : Parser Bag
parseBag =
    succeed Bag
        |= parseWord
        |. Parser.spaces
        |= parseWord


bagStr bag =
    bag.colourMod ++ " " ++ bag.colour


bagCountStr ( count, bag ) =
    String.fromInt count ++ " " ++ bagStr bag


parseWord : Parser String
parseWord =
    variable
        { start = Char.isLower
        , inner = Char.isLower
        , reserved = Set.empty
        }


part1 : List String -> String
part1 =
    List.map
        (\str ->
            str
                |> Parser.run parseRule
                >> Result.mapError (List.map deadEndToString >> String.join "\n")
                >> Result.map (\( bag, bagCounts ) -> bagStr bag ++ " " ++ (List.map bagCountStr bagCounts |> String.join "\t"))
                >> Result.Extra.merge
                |> (\resStr -> str ++ ": " ++ resStr)
        )
        >> String.join "\n"


part2 : List String -> String
part2 input =
    ""


inputTest : List String
inputTest =
    String.split "\n"
        """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""


inputReal : List String
inputReal =
    String.split "\n" """"""


deadEndToString : DeadEnd -> String
deadEndToString de =
    String.join "\t"
        [ "row:" ++ String.fromInt de.row
        , "col:" ++ String.fromInt de.col
        , "prob:" ++ problemToString de.problem
        ]


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting string ->
            "Expecting " ++ string

        ExpectingInt ->
            "ExpectingInt"

        ExpectingHex ->
            "ExpectingHex"

        ExpectingOctal ->
            "ExpectingOctal"

        ExpectingBinary ->
            "ExpectingBinary"

        ExpectingFloat ->
            "ExpectingFloat"

        ExpectingNumber ->
            "ExpectingNumber"

        ExpectingVariable ->
            "ExpectingVariable"

        ExpectingSymbol expectedSymbol ->
            "ExpectingSymbol" ++ " '" ++ expectedSymbol ++ "'"

        ExpectingKeyword string ->
            "ExpectingKeyword" ++ string

        ExpectingEnd ->
            "ExpectingEnd"

        UnexpectedChar ->
            "UnexpectedChar"

        Problem string ->
            "Problem" ++ string

        BadRepeat ->
            "BadRepeat"
