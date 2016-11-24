module Change where
import Data.List (sortBy,minimumBy)
import Data.Function (on)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins a c
    | a == 0    = Just []
    | null ans  = Nothing
    | otherwise = Just ans
  where
    ans = makeChange a c

-- swap in more efficient algorithim if we get a canonical set of coins
-- (more info below) otherwise, use slower algorithim.
makeChange :: Integer -> [Integer] -> [Integer]
makeChange amt coins =
    if isCanonical coins
    then changeGreedy amt (sortBy (flip compare) coins) []
    else minimumBy (compare `on` length) $ changeBrute amt coins

isCanonical :: [Integer] -> Bool
isCanonical coins =
    all (==True) $ map (\c -> c > sum (filter (<c) scoins)) scoins
  where
    scoins = sortBy (flip compare) coins

-- For canonical coin systems, you can use this fast algorithim
-- from wikipedia:
-- For the so-called canonical coin systems, like the one used in US and many other countries,
-- a greedy algorithm of picking the largest denomination of coin which is not greater than the
-- remaining amount to be made will produce the optimal result.[2] This is not the case for
-- arbitrary coin systems, though: if the coin denominations were 1, 3 and 4, then to make 6,
-- the greedy algorithm would choose three coins (4,1,1) whereas the optimal solution is
-- two coins (3,3).
changeGreedy :: Integer -> [Integer] -> [Integer] -> [Integer]
changeGreedy 0 _ acc  = acc
changeGreedy n [] acc = []
changeGreedy n xss@(x:xs) acc
    | x <= n    = changeGreedy (n-x) xss (x : acc)
    | otherwise = changeGreedy n xs acc

-- However, the tests use non-canonical coin systems too, so generate all possible combinations
-- and find the shortest when calling it
-- This is pretty slow for large numbers, there is a dynamic programming way to do this, also
-- mentioned in the wikipedia article
changeBrute :: Integer -> [Integer] -> [[Integer]]
changeBrute 0 _   = [[]]
changeBrute _ []  = []
changeBrute n xss@(x:xs)
    | n >= x    = map (x:) (changeBrute (n - x) xss) ++ changeBrute n xs
    | otherwise = changeBrute n xs
