module MultiDict exposing (..)

import Dict


type alias MultiDict comparable thing =
    Dict.Dict comparable (List thing)


empty : MultiDict comparable thing
empty =
    Dict.empty


insert : comparable -> thing -> MultiDict comparable thing -> MultiDict comparable thing
insert key value currDict =
    Dict.get key currDict
        |> Maybe.map
            (\currEntries ->
                Dict.insert key (List.append currEntries [ value ]) currDict
            )
        |> Maybe.withDefault (Dict.insert key [ value ] currDict)


fromList : (thing -> comparable) -> List thing -> Dict.Dict comparable (List thing)
fromList map things =
    things
        |> List.map (\thing -> ( map thing, thing ))
        |> fromEntryList


fromEntryList : List ( comparable, thing ) -> Dict.Dict comparable (List thing)
fromEntryList entries =
    List.foldl
        (\( key, value ) currDict -> insert key value currDict)
        Dict.empty
        entries
