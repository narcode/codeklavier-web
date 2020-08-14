module EasterEggs exposing (..)

magicnumbers : String -> List Int
magicnumbers string =
    case string of
      "africa" -> [8]
      "asia" -> [3, 19, 30, 76]
      "europe" -> [14, 100, 129, 130, 155]
      "oceania" -> [29]
      _ -> []
