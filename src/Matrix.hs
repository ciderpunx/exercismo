module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V
import Data.List (group)

data Matrix a = Matrix { cells :: V.Vector a
                       , rows  :: Int
                       , cols  :: Int
                       } deriving (Show, Eq)

mkMatrix :: (Int,Int) -> V.Vector a -> Matrix a
mkMatrix (r,c) xs
    | r*c == V.length xs = Matrix { cells = xs
                                  , rows  = r
                                  , cols  = c 
                                  }
    | otherwise          = error $ "I can't make a " 
                                   ++ show r
                                   ++ "x"
                                   ++ show c
                                   ++ " matrix using a vector of length "
                                   ++ show (V.length xs)

column :: Int -> Matrix a -> V.Vector a
column n m =
    if n>(-1) && n < cols m
    then V.fromList [ row r m V.! n | r <- [0..rows m-1]]
    else error $ "Column index " ++ show n ++ " outsite matrix bounds"

flatten :: Matrix a -> V.Vector a
flatten = cells

fromList :: [[a]] -> Matrix a
fromList xs =
    mkMatrix (rowLength, colLength) (V.fromList $ concat xs)
  where
    rowLength = length xs
    colLength
      | allSameLength = head lengths
      | null xs       = 0
      | otherwise     = error "Not all rows are the same length!"
    allSameLength = length (group lengths) == 1
    lengths = map length xs

fromString :: Read a => String -> Matrix a
fromString s =
    fromList . map parse $ lines s
  where
    parse s = case reads s of
      [] -> []
      (x,xs):_ -> x : parse xs

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newR,newC) m =
    if valid
    then mkMatrix (newR,newC) (cells m)
    else error $ "Invalid matrix dimensions " ++ show newR ++ "x" ++ show newC
  where
    valid = (newR*newC) == (rows m * cols m)

row :: Int -> Matrix a -> V.Vector a
row n m =
    if n > (-1) && (n * cols m) <= V.length cm - cols m
    then V.slice (n * cols m) (cols m) cm
    else error $ "Row index " ++ show n ++ " outside matrix bounds"
  where
    cm = cells m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m =
    mkMatrix (newR,newC) v
  where
    newR = cols m
    newC = rows m
    cvs = map (`column` m) [0..cols m-1]
    v   = V.concat cvs

egMatrix :: Matrix Int
egMatrix = mkMatrix (3,3) (V.fromList [0..8])
