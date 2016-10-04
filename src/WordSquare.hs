{-# LANGUAGE OverloadedStrings #-}
module WordSquare where
import Data.Char (isAlphaNum, toLower)
import Data.List.Split (chunksOf)

-- Convenience
encipherAndPrint :: String -> IO ()
encipherAndPrint =
    putStr . unlines . encipher

-- Given a string, give me back a list of strings
-- which are the rows of the ciphertext
encipher :: String -> [String]
encipher xs =
    if n == 0
      then chunksOf r e
      else chunksOf r e' ++ getChunks (r-1) e''
  where
    lp         = length $ normalizePlaintext xs
    e          = encipherSquare sq ""
    n          = (r * c) - lp               -- number of spaces we need to add
    (e',e'')   = splitAt (lp - (n*r) + n) e -- split our list into a normal part and a bit that needs spaces adding
    (c, r, sq) = square xs

-- Given a row length and a string
-- return me some rows of that length with spaces at the end of them
getChunks :: Int -> String -> [String]
getChunks r =
    map (++" ") . chunksOf r

-- Given a string, return a triplet of cols, rows and a grid
-- for that string
square :: String -> (Int, Int, [String])
square xs =
    (c, r, chunksOf c xs')
  where
    xs'  = normalizePlaintext xs
    lxs' = length xs'
    r    = numRows lxs'
    c    = numCols lxs'

-- Number of columns needed for the string of length
numCols :: Int -> Int
numCols = ceiling . sqrt . fromIntegral

-- Number of rows needed for the string of length
numRows :: Int -> Int
numRows = floor . sqrt . fromIntegral

-- Used for testing
squareSize :: String -> Int
squareSize = numCols . length

-- Strip nonalphanumerics and downcase
normalizePlaintext :: String -> String
normalizePlaintext =
    map toLower . filter isAlphaNum

-- Given a square/grid, and an accumulator, pull the first letter from each row
-- then recurse until no more rows left
encipherSquare :: [String] -> String -> String
encipherSquare xs out =
    if null $ head xs
      then filter isAlphaNum out
      else encipherSquare xs' (out ++ heads)
  where
    heads = map safeHead xs
    xs'   = map safeTail xs

-- safe head and tail give empty string or space instead of exploding
-- when no head or tail present.
safeTail :: String -> String
safeTail []    = ""
safeTail (_:xs) = xs

safeHead :: String -> Char
safeHead []    = ' '
safeHead (x:_) = x

-- Used for tests
plaintextSegments :: String -> [String]
plaintextSegments =
    map (filter isAlphaNum) . trd . square
  where
    trd (_,_,x) = x

-- Just used for tests
ciphertext :: String -> String
ciphertext =
    filter isAlphaNum . concat . encipher

-- Used for tests
normalizeCiphertext :: String -> String
normalizeCiphertext =
    unwords . map (filter isAlphaNum) . encipher
