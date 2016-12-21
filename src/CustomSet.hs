module CustomSet where
import Prelude hiding (null)
import Data.List (group, sort, (\\))

-- look into using GADTs to define this cf: https://stackoverflow.com/questions/7438600/datatypecontexts-deprecated-in-latest-ghc-why#7438716
data Set a = Empty
           | Set [a] deriving (Ord, Show, Eq)

fromList :: (Eq a, Ord a) => [a] -> Set a
fromList [] = Empty
fromList xs = Set . map head . group $ sort xs

null :: (Eq a, Ord a) => Set a -> Bool
null Empty = True
null _     = False

insert :: (Eq a, Ord a) =>   a -> Set a -> Set a
insert x Empty    = Set [x]
insert x (Set xs) = fromList (x:xs)

size :: (Eq a, Ord a) => Set a -> Int
size Empty    = 0
size (Set xs) = length xs

toList :: (Eq a, Ord a) => Set a -> [a]
toList Empty    = []
toList (Set xs) = xs

member :: (Eq a, Ord a) => a -> Set a -> Bool
member _ Empty    = False
member x (Set xs) = x `elem` xs

delete :: (Eq a, Ord a) => a -> Set a -> Set a
delete _ Empty    = Empty
delete x (Set xs) = Set $ filter (/=x) xs

difference :: (Eq a, Ord a) => Set a -> Set a -> Set a
difference Empty s = Empty
difference s Empty = s
difference (Set xs) (Set ys) = Set $ xs \\ ys

isDisjointFrom :: (Eq a, Ord a) => Set a -> Set a -> Bool
isDisjointFrom Empty Empty = True
isDisjointFrom Empty _     = True
isDisjointFrom _ Empty     = True
isDisjointFrom s1 s2       = null $ intersection s1 s2

isSubsetOf :: (Eq a, Ord a) => Set a -> Set a -> Bool
isSubsetOf Empty _ = True
isSubsetOf _ Empty = False
isSubsetOf (Set xs) (Set ys) = all (`elem` ys) xs

intersection :: (Eq a, Ord a) => Set a -> Set a -> Set a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Set xs) (Set ys) =
    let zs = filter (`elem` xs) ys
    in case zs of
      [] -> Empty
      _  -> Set zs

union :: (Eq a, Ord a) => Set a -> Set a -> Set a
union Empty Empty = Empty
union s Empty     = s
union Empty s     = s
union (Set xs) (Set ys) = fromList $ xs ++ ys
