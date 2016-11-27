module Sieve (primesUpTo) where
import Data.List.Ordered (minus)

primesUpTo :: Integer -> [Integer]
primesUpTo n =
    sieve (2 : [3,5..n])

-- with thanks to @warundeclared a more efficient actual sieve.
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =
    x : sieve (xs `minus` [x^2, x^2+x..])

sieveNaive :: [Integer] -> [Integer]
sieveNaive [] = []
sieveNaive (x:xs) =
    x : sieveNaive (filter ((/=0).(`mod`x)) xs)
