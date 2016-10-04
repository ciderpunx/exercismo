{-# LANGUAGE FlexibleInstances #-}
module SecretHandshake where
import Data.Digits (digitsRev)
import Text.Read (readMaybe)

class Handshakeable a where
    handshake :: a -> [String]

instance Handshakeable String where
    handshake xs =
      if all (\d -> d==1 || d==0) ds
        then toHandshake ds
        else []
      where
        ds  = strToIs xs

instance Handshakeable Int where
    handshake = toHandshake . digitsRev 2

toHandshake :: [Int] -> [String]
toHandshake []             = []
toHandshake [0]            = []
toHandshake (a:b:c:d:1:ds) = reverse $ toHandshake (a:b:c:d:0:ds)
toHandshake (1:ds)         = "wink" : toHandshake (0:ds)
toHandshake (_:1:ds)       = "double blink" : toHandshake (0:0:ds)
toHandshake (_:_:1:ds)     = "close your eyes" : toHandshake (0:0:0:ds)
toHandshake (_:_:_:1:ds)   = "jump" : toHandshake (0:0:0:0:ds)
toHandshake _              = []

strToIs :: String -> [Int]
strToIs xs =
    case xs' of
      Just n -> digitsRev 10 n
      Nothing -> []
  where
    xs' = readMaybe xs
