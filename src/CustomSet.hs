module CustomSet where
import Data.List (group, sort, (\\))

data Set a = Empty | Set [a] deriving (Ord, Show, Eq)

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

isDisjointFrom = undefined
isSubsetOf = undefined
intersection = undefined
union = undefined
