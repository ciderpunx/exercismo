module Strain where

keep _ []     = []
keep p (x:xs) = if p x then x : keep p xs else keep p xs

discard p = keep (not . p)

-- or jusr
keep'      = filter
discard' p = filter (not . p)
