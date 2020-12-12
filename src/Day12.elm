module Day12 exposing (main)

import AdventOfCode
import Array
import Result.Extra


main =
    AdventOfCode.day
        { dayNumber = 12
        , input = inputReal
        , testInput = inputTest
        , part1 = part1
        , part1TestExpected = Just "25"
        , part1Expected = Just "582"
        , part2 = part2
        , part2TestExpected = Just "286"
        , part2Expected = Just "52069"
        , debugWindows = \_ -> []
        }


type alias Boat =
    { position : ( Int, Int )
    , facing : Int
    , waypoint : ( Int, Int )
    }


directions =
    Array.fromList [ East, South, West, North ]


getDirection : Int -> Maybe Instruction
getDirection d =
    Array.get (modBy 4 d) directions


type Instruction
    = North
    | East
    | South
    | West
    | Forward
    | Left
    | Right


parseInstructions : List String -> Result String (List ( Instruction, Int ))
parseInstructions =
    List.map parseString >> Result.Extra.combine


parseString : String -> Result String ( Instruction, Int )
parseString str =
    let
        dirRes =
            case String.left 1 str of
                "N" ->
                    Ok North

                "E" ->
                    Ok East

                "S" ->
                    Ok South

                "W" ->
                    Ok West

                "F" ->
                    Ok Forward

                "L" ->
                    Ok Left

                "R" ->
                    Ok Right

                err ->
                    Err ("Not recognised: " ++ err)
    in
    case dirRes of
        Err err ->
            Err err

        Ok dir ->
            String.dropLeft 1 str
                |> String.toInt
                |> Result.fromMaybe "Could not parse int"
                |> Result.map (\i -> Tuple.pair dir i)


moveBoatPart1 : ( Instruction, Int ) -> Boat -> Result String Boat
moveBoatPart1 ( inst, num ) boat =
    case inst of
        North ->
            Ok { boat | position = boat.position |> Tuple.mapSecond ((+) num) }

        East ->
            Ok { boat | position = boat.position |> Tuple.mapFirst ((+) num) }

        South ->
            Ok { boat | position = boat.position |> Tuple.mapSecond (\p -> p - num) }

        West ->
            Ok { boat | position = boat.position |> Tuple.mapFirst (\p -> p - num) }

        Forward ->
            getDirection boat.facing
                |> Result.fromMaybe "No direction for boat facing"
                |> Result.map
                    (\dir ->
                        moveBoatPart1 ( dir, num ) boat
                    )
                |> Result.Extra.join

        Left ->
            changeDir ( inst, num ) boat

        Right ->
            changeDir ( inst, num ) boat


changeDir : ( Instruction, Int ) -> Boat -> Result String Boat
changeDir ( inst, i ) boat =
    case Debug.log "inst" inst of
        Left ->
            Debug.log "turned" <| turn (-) i boat

        Right ->
            Debug.log "turned" <| turn (+) i boat

        _ ->
            Err "Shouldn't be changing direction with this inst"


turn : (Int -> Int -> Int) -> Int -> Boat -> Result String Boat
turn mod degrees b =
    let
        _ =
            Debug.log "turning" ( mod, degrees, b )
    in
    case degrees of
        0 ->
            Ok b

        90 ->
            Ok { b | facing = (Debug.log "facingBeforemodBy" <| mod b.facing 1) |> modBy 4 }

        180 ->
            Ok { b | facing = mod b.facing 2 |> modBy 4 }

        270 ->
            Ok { b | facing = mod b.facing 3 |> modBy 4 }

        _ ->
            Err "Unexpected degrees"


part1 : List String -> String
part1 input =
    case parseInstructions input of
        Err err ->
            err

        Ok insts ->
            part1Loop (Boat ( 0, 0 ) 0 ( 10, 1 )) insts
                |> Result.map (\b -> (+) (Tuple.first b.position |> abs) (Tuple.second b.position |> abs) |> abs |> String.fromInt)
                |> Result.Extra.merge


part1Loop : Boat -> List ( Instruction, Int ) -> Result String Boat
part1Loop boat insts =
    case insts of
        [] ->
            Ok boat

        head :: next ->
            case moveBoatPart1 head boat of
                Err err ->
                    Err err

                Ok b ->
                    part1Loop b next


part2 : List String -> String
part2 input =
    case parseInstructions input of
        Err err ->
            err

        Ok insts ->
            part2Loop (Boat ( 0, 0 ) 0 ( 10, 1 )) insts
                |> Result.map (\b -> (+) (Tuple.first b.position |> abs) (Tuple.second b.position |> abs) |> abs |> String.fromInt)
                |> Result.Extra.merge


part2Loop : Boat -> List ( Instruction, Int ) -> Result String Boat
part2Loop boat insts =
    case insts of
        [] ->
            Ok boat

        head :: next ->
            case moveBoatPart2 head boat of
                Err err ->
                    Err err

                Ok b ->
                    part2Loop b next


moveBoatPart2 : ( Instruction, Int ) -> Boat -> Result String Boat
moveBoatPart2 ( inst, num ) boat =
    case inst of
        North ->
            Ok { boat | waypoint = boat.waypoint |> Tuple.mapSecond ((+) num) }

        East ->
            Ok { boat | waypoint = boat.waypoint |> Tuple.mapFirst ((+) num) }

        South ->
            Ok { boat | waypoint = boat.waypoint |> Tuple.mapSecond (\p -> p - num) }

        West ->
            Ok { boat | waypoint = boat.waypoint |> Tuple.mapFirst (\p -> p - num) }

        Forward ->
            Ok
                { boat
                    | position =
                        boat.waypoint
                            |> Tuple.mapBoth ((*) num) ((*) num)
                            |> Tuple.mapBoth ((+) <| Tuple.first boat.position) ((+) <| Tuple.second boat.position)
                }

        Left ->
            rotateWaypoint inst num boat

        Right ->
            rotateWaypoint inst num boat


rotateWaypoint : Instruction -> Int -> Boat -> Result String Boat
rotateWaypoint inst num boat =
    case Debug.log "rotate boat inst" inst of
        Left ->
            rotatePointAnticlockwise num boat.waypoint
                |> Result.map (\newW -> { boat | waypoint = newW })

        Right ->
            rotatePointClockwise num boat.waypoint
                |> Result.map (\newW -> { boat | waypoint = newW })

        _ ->
            Err "Shouldn't be changing direction with this inst"


rotatePointClockwise : Int -> ( Int, Int ) -> Result String ( Int, Int )
rotatePointClockwise degrees point =
    case degrees of
        0 ->
            Ok point

        90 ->
            Ok ( Tuple.second point, Tuple.first point |> (*) -1 )

        180 ->
            Ok ( Tuple.first point |> (*) -1, Tuple.second point |> (*) -1 )

        270 ->
            Ok ( Tuple.second point |> (*) -1, Tuple.first point )

        _ ->
            Err "Unexpected degrees"


rotatePointAnticlockwise : Int -> ( Int, Int ) -> Result String ( Int, Int )
rotatePointAnticlockwise degrees point =
    case degrees of
        0 ->
            Ok point

        90 ->
            Ok ( Tuple.second point |> (*) -1, Tuple.first point )

        180 ->
            Ok ( Tuple.first point |> (*) -1, Tuple.second point |> (*) -1 )

        270 ->
            Ok ( Tuple.second point, Tuple.first point |> (*) -1 )

        _ ->
            Err "Unexpected degrees"


inputTest : List String
inputTest =
    String.split "\n"
        """F10
N3
F7
R90
F11"""


inputReal : List String
inputReal =
    String.split "\n" """S3
R90
E1
S3
E5
S4
E5
N3
W1
N3
F91
W1
F49
W1
F7
R90
F13
S1
F87
L90
N5
R90
E1
F69
N4
F80
E2
F15
N5
F15
N4
E3
N2
R90
E4
N5
F71
W3
S4
R270
F39
N1
F100
R90
F52
S2
W3
L90
S1
F79
E5
R90
F94
E3
F87
N5
R90
F50
W5
N4
W4
S1
W2
R180
W4
F6
W2
N1
W4
F89
L90
E3
R90
F73
W1
N4
F77
E4
F42
N3
E5
R180
F51
E2
F22
N4
F95
L90
E2
F41
L90
F61
L180
E1
R90
N2
F32
E5
L90
L180
E3
F86
S3
L180
E3
F76
W1
F62
S2
F23
R90
F60
E2
F47
L90
S1
L270
N3
N2
W1
S4
S1
E5
F43
S4
R90
S5
N3
F100
W5
R90
S2
F30
E1
R180
R90
F1
R90
F32
N1
W3
R180
F9
N1
L90
E1
R90
W5
L180
N4
F57
F53
N4
E5
R90
E1
F32
R90
E5
L180
E1
L90
W4
S2
F77
N2
R90
S2
F68
S4
R90
E3
F66
R90
F85
S1
F47
F25
R90
N1
F65
R270
L270
F90
S3
F33
W5
N1
F19
E1
L90
F72
L90
F67
E3
R90
E4
R90
F6
S2
W5
F43
W1
F3
L270
N3
L90
E5
N3
F29
E4
R90
E4
L90
F32
E5
F52
N4
L270
N1
W5
R90
W5
N5
E3
N5
W2
L180
N2
E1
S4
N1
W4
F8
N2
W2
F80
N3
F98
L90
W3
L90
S2
L90
F98
L180
F12
E3
N2
F60
N5
W1
F54
W3
F74
E4
S3
R90
F94
L90
S2
L90
N4
E2
F83
R90
N2
E1
F86
E5
N1
R90
E5
R180
F72
L180
W5
L90
E4
L90
E1
F26
W4
F4
L90
W4
L90
E5
F92
R90
N1
L90
N5
F21
L90
L180
W3
F76
L90
F14
L90
E5
F50
S5
F18
W5
F27
S5
F52
L180
N3
L180
W2
N5
W3
S2
F48
W2
F57
R90
E3
R180
E4
F42
W2
F21
E2
S4
E2
N4
W4
S4
E3
F23
S1
W5
F70
S5
F77
W5
S3
L180
E4
L90
W2
L90
E1
F15
S4
F38
N1
L90
F27
N1
F34
L90
E1
S2
E2
L90
N3
F71
W4
S4
F14
R90
F58
R90
F8
R90
E1
L270
F12
E2
L90
F9
R90
S5
F48
S5
L90
E1
F7
W2
F97
S1
R90
N5
E2
N4
F20
R90
N1
R90
F84
R90
E3
S3
F34
S3
F41
R90
F15
L180
N1
F4
L180
W3
F50
S4
W3
F45
W1
S4
E2
L90
E5
R270
S3
L90
S4
F59
W4
R180
F10
E1
R90
S4
W5
L90
F45
S4
F92
R90
E5
F36
S1
L90
W1
S5
F30
W5
F80
N4
E2
R90
F98
F7
S5
W2
F1
N3
L180
N2
F49
E3
S5
S5
W4
E5
E3
S1
W1
F10
L90
F40
E4
N4
F25
R180
W5
N5
W1
S4
L90
F79
L180
S1
F100
S1
F34
S2
E4
L90
N2
F36
E1
N3
L90
F42
R90
W5
L270
F45
E4
F95
N5
F22
L90
F67
R180
S5
E5
R90
N1
R180
W3
F80
E4
F19
W5
N4
R90
N5
F28
F86
R180
R90
S2
E1
N5
W1
S3
W5
S3
F51
W5
S3
R90
R90
F35
W2
F51
N1
R180
S2
F37
R270
W2
S2
L90
L180
E4
N1
L180
E2
S1
F38
R90
N2
W1
R90
L180
E4
S3
R180
E3
S4
F65
E2
R90
N1
E4
L180
E5
R90
W2
F22
W5
N1
R90
E1
F34
S1
R90
N3
F62
F48
E5
F85
E5
S4
R90
E3
R90
E1
F54
S4
W3
W2
R90
N5
W4
N3
R180
E1
F88
W5
F49
E5
N4
W3
F81
R90
F58
N4
L90
F88
N3
F87
E2
N4
L90
N5
N1
F87
E1
L90
N3
W1
F13
F3
N4
L90
E3
S1
F35
S4
F1
L90
E1
N4
R90
F60
N2
W1
N1
R180
L180
S3
R90
E4
R180
W3
N1
W1
F3
L90
F8
W1
R90
S1
F84
L90
S3
F63
F11
E1
F38
R90
S3
R270
F50
R90
F15
L270
E1
S1
E5
L180
N4
L90
F37
N3
E5
F13
L180
E5
F88
E2
F9
W3
F6
N5
F75
N5
W5
S5
E3
R90
N5
E4
N3
L180
F1
R90
S3
E1
S4
E5
N3
R270
E3
L90
N3
S2
L90
S5
F90
R90
N1
R90
F50
F33
W3
F23
E3
R90
F81
E1
N3
R180
F16
S2
F35
R90
W5
F31
R90
S2
L180
E5
R180
F89
N1
L90
F68
W3
R90
W3
N1
L90
N4
L180
S5
R180
N4
R90
W5
N5
L180
N2
E4
N1
W4
F98
S5
F70
W5
F76"""
