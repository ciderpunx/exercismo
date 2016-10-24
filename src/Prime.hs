module Prime where

nth :: Int -> Maybe Integer
nth n =
    if n<1
    then Nothing
    else Just . head $ drop (n-1) primes

primes :: [Integer]
primes =
    2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
       | x < q   = x : sieve xs q ps
       | otherwise    =     sieve [x | x <- xs, x `mod` p > 0] (head t^2) t
