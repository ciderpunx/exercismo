module Brackets (arePaired) where
import Data.Char

type Stack = String

arePaired :: String -> Bool
arePaired = matchBrackets []

matchBrackets :: Stack -> String -> Bool
matchBrackets [] []    = True
matchBrackets stack [] = False -- unmatched opening bracket still on stack
matchBrackets stack (x:xs)
  | not (isBracket x)  = matchBrackets stack xs
  | isOpeningBracket x = matchBrackets (x:stack) xs
  | otherwise =  not (null stack)
                 && sameType (head stack) x
                 && matchBrackets (tail stack) xs

isBracket :: Char -> Bool
isBracket c = c `elem` "{}[]()"

isOpeningBracket :: Char -> Bool
isOpeningBracket c = c `elem` "{[("

sameType :: Char -> Char -> Bool
sameType x y =
    x == '[' && y == ']'
    || x == '{' && y == '}'
    || x == '(' && y == ')'
