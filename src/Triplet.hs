module Triplet where
import Data.List (sort)

mkTriplet :: Float -> Float -> Float -> (Float, Float, Float)
mkTriplet a b c = (a,b,c)

isPythagorean :: (Float, Float, Float) -> Bool
isPythagorean (a, b, c) =
    let [x,y,z] = sort [a,b,c]
    in  x^2 + y^2 == z^2

-- slow, naive implementation
pythagoreanTriplets' :: Float -> Float -> [(Float,Float,Float)]
pythagoreanTriplets' min max =
    [ (a,b,c) | a <- [min..max], b <- [a..max], c <- [b..max], a^2+b^2==c^2 ]

-- much faster; could be better still if we generated fewer
pythagoreanTriplets :: Float -> Float -> [(Float, Float,Float)]
pythagoreanTriplets min max =
    [ (a,b,c) | s <- [1..max]
              , t <- [s..max]
              , let r = sqrt (2 * s * t)
              , let a = r + s
              , let b = r + t
              , let c = r + s + t
              , isInt r
              , inRange a && inRange b && inRange c
              ]
  where
    inRange n = n<=max && n>=min
    isInt x   = abs (x - fromIntegral (ceiling x)) < 0.000001

answer :: Integer
answer =
    let (a,b,c) = theTripletWeWant
    in round $ a*b*c

theTripletWeWant :: (Float,Float,Float)
theTripletWeWant =
    head [ (a,b,c) | (a,b,c) <- pythagoreanTriplets 1 500, a+b+c == 1000]

