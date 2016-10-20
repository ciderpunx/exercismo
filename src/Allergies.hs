module Allergies where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes 
              | Chocolate
              | Pollen
              | Cats
              deriving (Show,Eq)

powersOfTwo :: [Integer]
powersOfTwo = map (2^) [0..]

allergenConstructors :: [Allergen]
allergenConstructors =
              [ Eggs
              , Peanuts
              , Shellfish
              , Strawberries
              , Tomatoes
              , Chocolate
              , Pollen
              , Cats
              ]

allergenLookup :: [(Integer, Allergen)]
allergenLookup =
    reverse $ zip powersOfTwo allergenConstructors 

allergies :: Integer -> [Allergen]
allergies = allergies' allergenLookup

-- I feel like this could be reduced to a fold. Another day.
allergies' :: [(Integer, Allergen)] -> Integer -> [Allergen]
allergies' _  0       = []
allergies' [] acc     = []
allergies' (x:xs) acc =
    if acc - fst x >= 0 
    then snd x : allergies' xs (acc - fst x)
    else allergies' xs acc

isAllergicTo :: Allergen -> Integer -> Bool
isAllergicTo allergen score =
    allergen `elem` allergies score
