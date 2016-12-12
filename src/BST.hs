module BST ( bstLeft
           , bstRight
           , bstValue
           , empty
           , fromList
           , insert
           , singleton
           , toList
           ) where
import Data.List (foldl')


data BST a = Empty
           | Tree (BST a) a (BST a) deriving (Show,Eq)

bstLeft Empty         = Nothing
bstLeft (Tree l _ _)  = Just l

bstRight Empty        = Nothing
bstRight (Tree _ _ r) = Just r

bstValue Empty        = Nothing
bstValue (Tree _ v _) = Just v

empty       = Empty

singleton x = Tree Empty x Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) Empty 

toList Empty        = []
toList (Tree l i r) = toList l ++ [i] ++ toList r

insert x Empty = singleton x
insert x (Tree l i r)
    | x <= i    = Tree (insert x l) i r
    | otherwise = Tree l i (insert x r)
