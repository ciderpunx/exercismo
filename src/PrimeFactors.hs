module PrimeFactors where

primeFactors :: Integer -> [Integer]
primeFactors = factors 2

factors :: Integer -> Integer -> [Integer]
factors d n
    | n == 1          = []
    | d * d > n       = [n]
    | n `mod` d == 0  = d : factors d (n `div` d)
    | otherwise       = factors (d + 1) n
