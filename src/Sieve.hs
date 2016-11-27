module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n =
    sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =
    x : sieve (filter (\n -> n`mod`x /= 0) xs)
