module Triangle where

data TriangleType = Equilateral
                  | Illegal
                  | Isosceles
                  | Scalene deriving (Eq, Show)

triangleType a b c
    |    a <= 0
      || b <= 0
      || c <= 0      = Illegal  -- any zero length sides
    |    a > (b + c)
      || b > (a + c)
      || c > (a + b) = Illegal  -- violates triangle inequality law
    |    a == b
      && b == c      = Equilateral
    |    a /= b
      && b /= c
      && a /= c      = Scalene
    | otherwise      = Isosceles
