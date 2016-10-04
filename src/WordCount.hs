module WordCount where
import Data.List (sort, group)
import Data.Char (isAlphaNum, toLower)
import Data.Map (fromList)

wordCount xs =
    fromList [ (head x, length x) | x <- group . sort . map (map toLower) $ words' xs]

words' :: String -> [String]
words' s =
    case dropWhile (not . isAlphaNum) s of
      "" -> []
      s' -> w : words' s''
            where (w, s'') = span isAlphaNum s'
