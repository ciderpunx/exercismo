module PigLatin (translate) where
import Data.Char (isLetter)

translate :: String -> String
translate =
    unwords . map (trWord . filter isLetter). words

trWord :: String -> String
trWord "" = ""
trWord xss@('y':'t':xs)   = xss ++ "ay"
trWord xss@('x':'r':xs)   = xss ++ "ay"
trWord xss@(x:'q':'u':xs) = if x `elem` vowels then xss ++ "ay" else xs ++ (x:"quay")
trWord ('q':'u':xs)       = xs ++ "quay"
trWord xss@(x:xs) =
    if x `elem` vowels
    then xss ++ "ay"
    else let (cs,rs) = span (`elem` consonants) xs
         in  rs ++ (x:cs) ++ "ay"

letters, vowels, consonants :: String
letters = ['a'..'z']
vowels = "aeiou"
consonants = filter (`notElem` vowels) letters
