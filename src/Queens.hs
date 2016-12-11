module Queens (boardString, canAttack) where
import Data.List (intersperse)

canAttack :: (Int,Int) -> (Int,Int) -> Bool
canAttack a b = sameRank a b
             || sameFile a b
             || sameDiag a b

sameRank :: (Int,Int) -> (Int,Int) -> Bool
sameRank (a,_) (b,_) = a == b

sameFile :: (Int,Int) -> (Int,Int) -> Bool
sameFile (_,a) (_,b) = a == b

sameDiag :: (Int,Int) -> (Int,Int) -> Bool
sameDiag (a,b) (a',b') =
    abs(a-a') == abs(b-b')

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing =
    format us
boardString Nothing (Just (a,b)) =
    format $ replaceAt (a, b) 'B' us
boardString (Just (a,b)) Nothing =
    format $ replaceAt (a, b) 'W' us
boardString (Just (a,b)) (Just (a',b')) =
    format $ replaceAt (a,b) 'W' $ replaceAt (a',b') 'B' us

us :: [String]
us =
    replicate 8 (replicate 8 '_')

format :: [String] -> String
format =
    unlines . map (intersperse ' ')

replaceAt :: (Int,Int) -> Char -> [String] -> [String]
replaceAt (a,b) n xs =
    l ++ (m ++ (n : o)) : p
  where
    (l, y:p) = splitAt a xs
    (m, _:o) = splitAt b y
