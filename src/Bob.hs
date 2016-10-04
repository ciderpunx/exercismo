module Bob where
import Data.Char (isSpace, isUpper, isLetter)

responseFor :: String -> String
responseFor xs
    | silence   = "Fine. Be that way!"
    | shouting  = "Whoa, chill out!"
    | question  = "Sure."
    | otherwise = "Whatever."
  where
    xs'      = filter (not . isSpace) xs
    letters  = filter isLetter xs'
    shouting = all isUpper letters && not (null letters)
    silence  = null xs'
    question = last xs' == '?'
