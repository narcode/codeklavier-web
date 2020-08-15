module EasterEggs exposing (..)

magicnumbers : String -> List Int
magicnumbers string =
    case string of
      "africa" -> [8]
      "asia" -> [3, 30, 76]
      "europe" -> [14, 100, 129, 130, 155]
      "oceania" -> [29]
      _ -> []

isVideo : String -> Bool
isVideo string =
    let
        videos = [19, 130]
        toint = String.toInt string
    in
    case toint of
      Just int ->
        if List.member int videos then
          True
        else
          False
      Nothing -> False
