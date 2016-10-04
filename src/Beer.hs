module Beer where

sing :: Int -> Int -> String
sing e s =
    unlines . map verse $ reverse [s..e]

verse :: Int -> String
verse 0 =
    ucbs ++ location ++ ", " ++ bs ++ getMore ++ bottles 99 ++ location ++ ".\n"
  where
    bs   = bottles 0
    ucbs = "N" ++ tail bs

verse n =
    if n>0
    then bs ++ location ++ ", " ++ bs ++ drink n ++ bottles n' ++ location ++ ".\n"
    else error "You cannot have fewer than no bottles of beer. Not even if you are a really heavy drinker."
  where
    bs = bottles n
    n' = n-1

bottles :: Int -> String
bottles 0 = "no more bottles of beer"
bottles 1 = "1 bottle of beer"
bottles n = show n ++ " bottles of beer"

location :: String
location  = " on the wall"

drink :: Int -> String
drink 1   = ".\nTake it down and pass it around, "
drink _   = ".\nTake one down and pass it around, "

getMore :: String
getMore   = ".\nGo to the store and buy some more, "
