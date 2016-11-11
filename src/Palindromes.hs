module Palindromes where

-- Take advantage of Haskell's laziness by only computing the first large palindrome.
-- In the worst case we have to cycle through all pairs of numbers from max to max-min
-- i.e. complexity is O(min^2). This is acceptable for ranges of 1..6 digits in ghci, 
-- probably higher if compiled
largestPalindrome :: Integer -> Integer -> (Integer, [(Integer,Integer)])
largestPalindrome min max =
    if max < 10
    then (max, factorsPairsRange max min max)
    else head [ (p,[(x,y)]) | x <- [max,max-1..max-min]
                            , y <- [x,x-1..max-min]
                            , let p=x*y
                            , isPal p ]


-- O(min^2)
smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer,Integer)])
smallestPalindrome min max =
    if max < 10
    then (min, factorsPairsRange min min max)
    else head [ (p,[(x,y)]) | x <- [min..max]
                            , y <- [x..max]
                            , let p=x*y
                            , isPal p ]

factorsPairsRange n min max =
    [ (x,y) | x <- [min..max]
            , n `mod` x == 0
            , let y = n`div`x
            , x>=min && x<=max
            , y>=x && y<=max
            ]

-- Faster than using Data.Digits
isPal :: Integer -> Bool
isPal n = show n == reverse (show n)
