module Allergies where
import Data.Digits (digitsRev)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Show,Eq)

isAllergicTo :: Allergen -> Integer -> Bool
isAllergicTo allergen score =
    allergen `elem` allergies score

-- Convert to binary digits and zip with the list of constructors
-- where there's a 1 that constructor should be in the returned value
-- otherwise there shouldn't be. Can be expressed as a fold, but this
-- is shorter.
allergies :: Integer -> [Allergen]
allergies n =
    map snd . filter ((==1) . fst) $ zip (digitsRev 2 n) allergenConstructors

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
