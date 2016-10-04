module Sublist where

data Sublist = Equal
             | Sublist
             | Superlist
             | Unequal
             deriving (Show, Ord, Eq)

xs `sublist` ys
    | xs == ys         = Equal
    | xs `contains` ys = Superlist
    | ys `contains` xs = Sublist
    | otherwise        = Unequal

xs `contains` ys =
       ys == take (length ys) xs
    || length xs >= length ys && tail xs `contains` ys
