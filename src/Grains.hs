module Grains where
--import Control.Monad (sequence)
import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n
    | n < 1     = Nothing
    | n > 64    = Nothing
    | otherwise = Just $ 2^(n-1)

total :: Integer
total =
    fromJust . fmap sum $ traverse square [1..64]
    -- fromJust . fmap sum . sequence $ map square [1..64]
    -- fromJust . foldM (fmap . (+)) 0 $ map square [1..64]

keep, discard :: (a -> Bool) -> [a] -> [a]
keep p = foldr (\x ys -> if p x then x:ys else ys) []
-- Not allowed to use filter, so cannot keep = filter p
discard p = keep (not . p)
