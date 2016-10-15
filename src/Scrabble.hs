module Scrabble where
import Data.Char (toLower)

scoreLetter :: Char -> Int
scoreLetter x
    | y `elem` "aeioulnrst" =  1
    | y `elem` "dg"         =  2
    | y `elem` "bcmp"       =  3
    | y `elem` "fhvwy"      =  4
    | y `elem` "k"          =  5
    | y `elem` "jx"         =  8
    | y `elem` "qz"         = 10
    | otherwise             =  0
  where
    y = toLower x

scoreWord :: String -> Int
scoreWord   = sum . map scoreLetter
