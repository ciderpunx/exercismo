module SumOfMultiples where
import Data.List (group, sort)

-- Given a list of ints and a top bounds find unique multiples of each of the elements of the list up to
-- the top bound (but not including it), and sum them. It seems that map head . group . sort may be more efficient
-- than nub -- O(n log n) rather than O(n^2). If we need better than maybe use Data.Set
-- sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples ms x =
    sum . map head . group . sort $ foldr (\m ns -> ns ++ [m, m*2 .. x-1]) [] ms
