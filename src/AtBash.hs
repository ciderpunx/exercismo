module AtBash where
import Data.Map (Map,(!),fromList)
import Data.Char (toLower, isAlphaNum, isAscii)
import Data.List.Split (chunksOf)

alphabet,key,numbers :: String
a2k,k2a :: Map Char Char
encode, decode, format, validInp :: String -> String

alphabet = ['a' .. 'z']
key      = reverse alphabet
numbers  = ['0' .. '9']
a2k      = fromList $ zip (alphabet ++ numbers) (key ++ numbers)
k2a      = fromList $ zip (key ++ numbers) (alphabet ++ numbers)
encode   = format . map (a2k !) . validInp
validInp = filter isAscii . map toLower . filter isAlphaNum
format   = unwords . chunksOf 5
decode   = map (k2a !) . filter isAlphaNum
