module CustomSet where
import Prelude hiding (null)
import Data.List (group, sort, (\\))

data Set a = Empty
           | Set [a] deriving (Ord, Show, Eq)

fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList xs = Set . map head . group $ sort xs

null Empty = True
null _     = False

insert x Empty    = Set [x]
insert x (Set xs) = fromList (x:xs)

size Empty    = 0
size (Set xs) = length xs

toList Empty    = []
toList (Set xs) = xs

member _ Empty    = False
member x (Set xs) = x `elem` xs

delete _ Empty    = Empty
delete x (Set xs) = Set $ filter (/=x) xs

difference Empty s = Empty
difference s Empty = s
difference (Set xs) (Set ys) = Set $ xs \\ ys

isDisjointFrom Empty Empty = True
isDisjointFrom Empty _     = True
isDisjointFrom _ Empty     = True
isDisjointFrom s1 s2       = null $ intersection s1 s2

isSubsetOf Empty _ = True
isSubsetOf _ Empty = False
isSubsetOf (Set xs) (Set ys) = all (`elem` ys) xs

intersection Empty _ = Empty
intersection _ Empty = Empty
intersection (Set xs) (Set ys) =
    let zs = filter (`elem` xs) ys
    in case zs of
      [] -> Empty
      _  -> Set zs

union Empty Empty = Empty
union s Empty     = s
union Empty s     = s
union (Set xs) (Set ys) = fromList $ xs ++ ys
