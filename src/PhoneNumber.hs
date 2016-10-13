module PhoneNumber where
import Data.Char (isDigit)

areaCode :: String -> Maybe String
areaCode xs =
    take 3 <$> number xs

prettyPrint :: String -> Maybe String
prettyPrint xs =
    fmt <$> number xs
  where
    fmt (a:b:c:d:e:f:xs) = "(" ++ [a,b,c] ++ ") "
                           ++ [d,e,f] ++ "-"
                           ++ xs

number :: String -> Maybe String
number xs
    | length ds < 10    = Nothing
    | length ds == 10   = Just ds
    | length ds == 11
      && head ds == '1' = Just $ tail ds
    | otherwise         = Nothing
  where
    ds = filter isDigit xs
