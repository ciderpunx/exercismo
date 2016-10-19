module Series where
import Data.Char (digitToInt, isDigit)

largestProduct :: Int -> String -> Maybe Int
largestProduct n xs
    | n > length xs  = Nothing
    | n < 0          = Nothing
    | n == 0         = Just 1
    | all isDigit xs = max (Just $ product nextNDigits)
                           (largestProduct n (tail xs))
    | otherwise      = Nothing
  where
    nextNDigits = map digitToInt $ take n xs
