module Anagram where
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor w =
    filter (\x -> lc x /= lc w && isort x == isort w)
  where
    isort = sort . lc
    lc    = map toLower
