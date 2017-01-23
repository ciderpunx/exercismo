module Say where
import Data.List (splitAt)

toUnits [] = []
toUnits (x:xs) =
    case length xs of
      3 -> (show x ++ " million ")  : toUnits xs
      2 -> (show x ++ " thousand ") : toUnits xs
      1 -> (show x ++ " hundred ")  : toUnits xs
      0 -> [show x]

toWords [] = []
toWords (x:[])
  | x == '1' = "one"
  | x == '2' = "two"
  | x == '3' = "three"
  | x == '4' = "four"
  | x == '5' = "five"
  | x == '6' = "six"
  | x == '7' = "seven"
  | x == '8' = "eight"
  | x == '9' = "nine"
toWords (x:y:[])
  | [x,y] == "11" = "eleven"
  | [x,y] == "12" = "twelve"
  | [x,y] == "13" = "thirteen"
  | [x,y] == "14" = "fourteen"
  | [x,y] == "15" = "fifteen"
  | [x,y] == "16" = "sixteen"
  | [x,y] == "17" = "seventeen"
  | [x,y] == "18" = "eighteen"
  | [x,y] == "19" = "nineteen"
  | [x,y] == "20" = "twenty"
  | x     == '2'  = "twenty-" ++ toWords [y]
  | [x,y] == "30" = "thirty"
  | x     == '3'  = "thirty-" ++ toWords [y]
  | [x,y] == "40" = "forty"
  | x     == '4'  = "forty-" ++ toWords [y]
  | [x,y] == "50" = "fifty"
  | x     == '5'  = "fifty-" ++ toWords [y]
  | [x,y] == "60" = "sixty"
  | x     == '6'  = "sixty-" ++ toWords [y]
  | [x,y] == "70" = "seventy"
  | x     == '7'  = "seventy-" ++ toWords [y]
  | [x,y] == "80" = "eighty"
  | x     == '8'  = "eighty-" ++ toWords [y]
  | [x,y] == "90" = "ninety"
  | x     == '9'  = "ninety-" ++ toWords [y]

chunksOf :: Int -> String -> [String]
chunksOf n xs
    | length xs <= n = [xs]
    | otherwise      = take n xs : chunksOf n (drop n xs)
