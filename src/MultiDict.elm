module MultiDict exposing (..)

import Dict


fromList : (thing -> comparable) -> List thing -> Dict.Dict comparable (List thing)
fromList map things =
    things
        |> List.map (\thing -> ( map thing, thing ))
        |> fromEntryList


fromEntryList : List ( comparable, thing ) -> Dict.Dict comparable (List thing)
fromEntryList entries =
    List.foldl
        (\( key, value ) currDict ->
            Dict.get key currDict
                |> Maybe.map
                    (\currEntries ->
                        Dict.insert key (value :: currEntries) currDict
                    )
                |> Maybe.withDefault (Dict.insert key [ value ] currDict)
        )
        Dict.empty
        entries
