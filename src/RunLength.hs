module RunLength where 
import Data.List (group)
import Data.Char (isNumber)

data RLE = RLE Int Char

instance Show RLE where
    show (RLE l c)
      | l == 1    = [c]
      | otherwise = show l ++ [c]

encode :: String -> String
encode xs =
   concatMap (\x -> show $ RLE (length x) (head x)) $ group xs

decode :: String -> String
decode "" = ""
decode xs =
    if null c
    then y : decode ys
    else replicate (read c) y ++ decode ys
  where
    (c, y:ys) = span isNumber xs
