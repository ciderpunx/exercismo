module FoodChain where
import qualified Data.Map as M
import Data.Maybe (fromJust)

song :: String
song = concat $
        verse True "fly" :
          map (verse True) [ "spider"
                           , "bird"
                           , "cat"
                           , "dog"
                           , "goat"
                           , "cow"
                           , "horse"
                           ]

swallowed :: String -> String
swallowed a =
    unlines [ "I know an old lady who swallowed a " ++ a ++ "."
            , fromJust (M.lookup a phrases)
            ]

catch :: String -> String
catch a =
    let foodstuff = fromJust (M.lookup a eats)
    in "She swallowed the " ++ a ++ " to catch the " ++
          ( if foodstuff == "spider"
            then "spider that wriggled and jiggled and tickled inside her"
            else foodstuff ) ++ ".\n"

verse :: Bool -> String -> String
verse isVerse "fly" =
    if isVerse
    then swallowed "fly"
    else fromJust (M.lookup "fly" phrases) ++ "\n"

verse _ "horse" = swallowed "horse"

verse isVerse animal =
    (if isVerse then swallowed animal else "")
    ++ catch animal
    ++ verse False (fromJust (M.lookup animal eats))

eats :: M.Map String String
eats =
    M.fromList [ ("spider","fly")
               , ("bird","spider")
               , ("cat","bird")
               , ("dog","cat")
               , ("goat","dog")
               , ("cow","goat")
               ]

phrases :: M.Map String String
phrases =
    M.fromList [ ("fly","I don't know why she swallowed the fly. Perhaps she'll die.\n")
               , ("spider","It wriggled and jiggled and tickled inside her.")
               , ("bird","How absurd to swallow a bird!")
               , ("cat","Imagine that, to swallow a cat!")
               , ("dog","What a hog, to swallow a dog!")
               , ("goat","Just opened her throat and swallowed a goat!")
               , ("cow","I don't know how she swallowed a cow!")
               , ("horse", "She's dead, of course!")
               ]
