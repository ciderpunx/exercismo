module Raindrops where

convert :: Integer -> String
convert n =
    if null pling && null plang && null plong
      then show n
      else pling ++ plang ++ plong
  where
    fs    = factors n
    pling = if 3 `elem` fs then "Pling" else ""
    plang = if 5 `elem` fs then "Plang" else ""
    plong = if 7 `elem` fs then "Plong" else ""

-- for the purposes of this n is a factor of itself
factors :: Integer -> [Integer]
factors n =
    n : filter ((==0) . mod n) [1..n`div`2]
