module OcrNum where
import Data.List.Split (chunksOf)
import Data.List (transpose, intercalate)

-- The basic approach is to get rows of cells and to be able to extract
-- our digits from the cells. This requires rather a lot of list juggling
-- especially in the numsToStr function where we have to go via a triple.
-- But that operation isn't really part of the question, I just wanted to 
-- be able to go both ways with the conversion.

type Grid a = [a]
type Matrix a = Grid (Grid a)

convert :: String -> String
convert = intercalate "," . strToNums

-- The conversion function strToNums and numsToStr should be inverses of each other st.
--  testData == (numsToStr $ strToNums testData)
-- However, we don't reconstruct the blanks in empty lines so we have to make do with 
-- this weaker property:
--   strToNums testData == (strToNums . numsToStr $ strToNums testData)
strToNums :: String -> Matrix Char
strToNums =
    map (map cellToNum . cells) . rows

cells :: Grid String -> Grid String
cells =
    map concat . chunksOf 3 . concatMap (chunksOf 3) . transpose

rows :: String -> Matrix String
rows =
    map init . chunksOf 4 . lines

-- I wonder if this can be simplified?
numsToStr :: Matrix Char -> String
numsToStr =
    unlines . map (fmt . unzip3 . map (toTriple . cellToDigit . numToCell))
  where
    fmt (a,b,c)         = unlines (map concat [a,b,c])
    toTriple (x:y:z:[]) = (x,y,z)

rowsToCells :: Matrix String -> Matrix String
rowsToCells =
    map cells

cellToDigit :: String -> Grid String
cellToDigit =
    transpose . chunksOf 3

numToCell n
    | n == '0' = " ||_ _ ||"
    | n == '1' = "       ||"
    | n == '2' = "  |___ | "
    | n == '3' = "   ___ ||"
    | n == '4' = " |  _  ||"
    | n == '5' = " | ___  |"
    | n == '6' = " ||___  |"
    | n == '7' = "   _   ||"
    | n == '8' = " ||___ ||"
    | n == '9' = " | ___ ||"
    | otherwise = error $ "Couldn't convert to cell, only 0..9 implemented\n" ++ show n

cellToNum xs
    | xs == " ||_ _ ||" = '0'
    | xs == "       ||" = '1'
    | xs == "  |___ | " = '2'
    | xs == "   ___ ||" = '3'
    | xs == " |  _  ||" = '4'
    | xs == " | ___  |" = '5'
    | xs == " ||___  |" = '6'
    | xs == "   _   ||" = '7'
    | xs == " ||___ ||" = '8'
    | xs == " | ___ ||" = '9'
    | otherwise         = '?' -- error $ "Couldn't parse number string was:\n" ++ xs

testData = concat
    [ " _ \n"
    , "| |\n"
    , "|_|\n"
    , "   \n"
    , "    _  _     _  _  _  _  _  _ \n"
    , "  | _| _||_||_ |_   ||_||_|| |\n"
    , "  ||_  _|  | _||_|  ||_| _||_|\n"
    , "                              \n"
    , "    _     _  _  _  _  _ \n"
    , "  | _||_||_   ||_||_|| |\n"
    , "  ||_   | _|  ||_| _||_|\n"
    , "\n"
    ]
