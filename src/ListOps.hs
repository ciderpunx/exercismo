{-# Language BangPatterns #-}
module ListOps where
import Prelude hiding
    ( (++)
    , concat
    , foldr
    , length
    , map
    , reverse
    )

a ++ [] = a
[] ++ b = b
(x:xs) ++ ys = x : (xs ++ ys)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b 
foldl' f !acc [] = acc
foldl' f !acc (x:xs) = foldl' f (f acc x) xs

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

length :: [a] -> Int
length = foldr (\_ x -> x+1) 0

concat :: [[a]] -> [a]
concat = foldr (++) []

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

-- Or write it by hand like this.
reverse' :: [a] -> [a]
reverse' =
    rev []
  where
    rev acc [] = acc
    rev acc (x:xs) = rev (x:acc) xs
