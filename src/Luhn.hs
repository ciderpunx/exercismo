module Luhn (addends, checkDigit, checksum, create, isValid) where
import Data.Digits

checkDigit :: Integer -> Integer
checkDigit = last . digits 10

addends :: Integer -> [Integer]
addends =
      reverse . dblEveryOther . digitsRev 10
  where
    dblEveryOther = everyOther' (clean . (*2))
    clean n       = if n < 10 then n else n - 9

checksum :: Integer -> Integer
checksum n = sum (addends n) `mod` 10

isValid :: Integer -> Bool
isValid = (==0) . checksum

-- this is sort of cheeky, try all the digits and grab the first one that is
-- valid. Worst case performance only requires 10 appends, digits and undigits
-- so its fine to do it this way.
create :: Integer -> Integer
create n =
    head [ x | d <- [0..9]
             , let x = unDigits 10 (digits 10 n ++ [d])
             , isValid x
         ]

-- Just because it is nicer to do it this way
everyOther, everyOther' :: (a -> a) -> [a] -> [a]
everyOther f  = zipWith ($) (cycle [f,id])
everyOther' f = zipWith ($) (cycle [id,f])
