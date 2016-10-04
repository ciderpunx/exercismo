module DNA where

toRNA :: String -> Maybe String
toRNA = traverse d2r

d2r :: Char -> Maybe Char
d2r 'G' = Just 'C'
d2r 'C' = Just 'G'
d2r 'T' = Just 'A'
d2r 'A' = Just 'U'
d2r _   = Nothing
