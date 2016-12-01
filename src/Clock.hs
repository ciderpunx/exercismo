module Clock (toString, fromHourMin) where

data Clock =
    Clock { hour :: Integer
          , minute :: Integer
          } deriving Eq

instance Show Clock where
  show c = zeropad (hour c) ++ ":" ++ zeropad (minute c)

instance Num Clock where
  (+) (Clock h1 m1) (Clock h2 m2) =
        Clock { hour   = (h1 + h2 + (m1+m2) `div` 60) `mod` 24
              , minute = (m1 + m2) `mod` 60
              }

  (*) (Clock h1 m1) (Clock h2 m2) =
        Clock { hour   = (h1 * h2 * (1 + (m1*m2) `div` 60)) `mod` 24
              , minute = (m1 * m2) `mod` 60
              }
  abs c  = c           -- all clock times are positive, return self

  signum c = Clock 1 1 -- positive identity, to satisfy abs x * signum x == x
                       -- per: https://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html#v:signum

  negate (Clock h m) =
      Clock { hour   = 23 - h - (m `div` 60)
            , minute = 60 - m
            }

  fromInteger n = Clock 0 0 + Clock 0 n

-- We can simply use logic from Clock instances to implement our functions...
toString :: Clock -> String
toString = show

fromHourMin :: Integer -> Integer -> Clock
fromHourMin h m = Clock 0 0 + Clock h m

zeropad :: (Num a, Ord a, Show a) => a -> String
zeropad n =
    if n < 10
    then '0' : show n
    else show n
