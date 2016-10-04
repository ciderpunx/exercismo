module Hamming where

hamming xs ys =
    sum $ zipWith (\x y -> if x /= y then 1 else 0) xs ys
