module EasterEggs exposing (..)

magicnumbers : String -> List Int
magicnumbers string =
    case string of
      "africa" -> [8, 16, 24, 25, 50, 60, 182]
      "antarctica" -> [55, 90]
      "asia" -> [3, 19, 30, 76, 77]
      "europe" -> [9, 14, 100, 123, 130, 155]
      "oceania" -> [25, 29]
      "northamerica" -> [17, 80, 101, 212, 302]
      "southamerica" -> [25, 30]
      _ -> []

isVideo : String -> Bool
isVideo string =
    let
        videos = [19, 60, 130]
        toint = String.toInt string
    in
    case toint of
      Just int ->
        if List.member int videos then
          True
        else
          False
      Nothing -> False
