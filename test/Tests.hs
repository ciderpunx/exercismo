-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE DeriveAnyClass #-}
--import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
--import System.Exit (ExitCode(..), exitWith)
import Data.Foldable     (for_)
import Data.Time.Calendar (fromGregorian)
--import Control.Exception (Exception, throw, evaluate)
import Control.Monad (unless)
import Test.Hspec        (Spec, describe, it, shouldBe, expectationFailure, shouldThrow, shouldSatisfy)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
-- import Data.Either       (isLeft)
--import Data.Map          (fromList)
--import Data.List (isPrefixOf)
--import Anagram (anagramsFor)
--import Beer (sing, verse)
--import Data.Map (fromList)
--import Data.Char (toUpper)
--import WordSquare (normalizePlaintext, squareSize, plaintextSegments, ciphertext, normalizeCiphertext)
--import SumOfMultiples (sumOfMultiples)
--import Bob (responseFor)
--import SpaceAge (Planet(..), ageOn)
--import Accumulate (accumulate)
--import AtBash (encode, decode)
--import Grains (square, total)
--import Sublist (Sublist(Equal,Sublist,Superlist,Unequal), sublist)
--import DNA (toRNA)
--import SecretHandshake (handshake)
--import DNACount (count, nucleotideCounts)
--import Wordy2 (answer)
-- import RobotSimulator
-- import Leap (isLeapYear)
-- import Strain (keep, discard)
-- import PhoneNumber (areaCode, number, prettyPrint)
--import School (add, empty, grade, sorted)
--import LinkedList
--  ( datum
--  , fromList
--  , isNil
--  , next
--  , new
--  , nil
--  , reverseLinkedList
--  , toList
--  )
--import Prelude hiding
--    ( (++)
--    , concat
--    , foldr
--    , length
--    , map
--    , reverse
--    )
--
--import ListOps
--    ( (++)
--    , concat
--    , foldl'
--    , foldr
--    , length
--    , map
--    , reverse
--    )
--import Etl (transform)
--import Scrabble (scoreLetter, scoreWord)
--import Triangle
--  ( TriangleType ( Equilateral
--                 , Illegal
--                 , Isosceles
--                 , Scalene
--                 )
--  , triangleType
--  )
--import Raindrops (convert)
--import PrimeFactors (primeFactors)
-- import Series (largestProduct)
--import Allergies
--  ( Allergen ( Cats
--             , Chocolate
--             , Eggs
--             , Peanuts
--             , Pollen
--             , Shellfish
--             , Strawberries
--             , Tomatoes
--             )
--  , allergies
--  , isAllergicTo
--  )
--import Prime (nth)
-- import Triplet (isPythagorean, mkTriplet, pythagoreanTriplets)
-- import Pascal (rows)
--import OcrNum (convert)
--import FoodChain (song)
import Meetup (Weekday(..), Schedule(..), meetupDay)

-- Meetup
main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "meetup" $
          describe "meetupDay" $ for_ cases test
  where

    test Case{..} = it description assertion
      where
        assertion   = returnedDay `shouldBe` expectedDay
        returnedDay = meetupDay week dayofweek year month
        expectedDay = fromGregorian year month dayofmonth

-- Test cases adapted from `exercism/x-common/meetup.json` on 2016-07-26.

data Case = Case { description :: String
                 , year        :: Integer
                 , month       :: Int
                 , week        :: Schedule
                 , dayofweek   :: Weekday
                 , dayofmonth  :: Int
                 }

cases :: [Case]
cases = [ Case { description = "monteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 13
               }
        , Case { description = "monteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 19
               }
        , Case { description = "monteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Monday
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 19
               }
        , Case { description = "tuesteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 16
               }
        , Case { description = "tuesteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Tuesday
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 16
               }
        , Case { description = "wednesteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 13
               }
        , Case { description = "wednesteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Wednesday
               , dayofmonth  = 19
               }
        , Case { description = "thursteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 16
               }
        , Case { description = "thursteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 13
               }
        , Case { description = "thursteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Thursday
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 19
               }
        , Case { description = "friteenth of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 16
               }
        , Case { description = "friteenth of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Teenth
               , dayofweek   = Friday
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 16
               }
        , Case { description = "saturteenth of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 13
               }
        , Case { description = "saturteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Saturday
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 19
               }
        , Case { description = "sunteenth of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 16
               }
        , Case { description = "sunteenth of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Teenth
               , dayofweek   = Sunday
               , dayofmonth  = 13
               }
        , Case { description = "first Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Monday
               , dayofmonth  = 4
               }
        , Case { description = "first Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Monday
               , dayofmonth  = 1
               }
        , Case { description = "first Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = First
               , dayofweek   = Tuesday
               , dayofmonth  = 7
               }
        , Case { description = "first Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = First
               , dayofweek   = Tuesday
               , dayofmonth  = 4
               }
        , Case { description = "first Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = First
               , dayofweek   = Wednesday
               , dayofmonth  = 3
               }
        , Case { description = "first Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = First
               , dayofweek   = Wednesday
               , dayofmonth  = 7
               }
        , Case { description = "first Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = First
               , dayofweek   = Thursday
               , dayofmonth  = 5
               }
        , Case { description = "first Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = First
               , dayofweek   = Thursday
               , dayofmonth  = 3
               }
        , Case { description = "first Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 1
               }
        , Case { description = "first Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 6
               }
        , Case { description = "first Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = First
               , dayofweek   = Saturday
               , dayofmonth  = 5
               }
        , Case { description = "first Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = First
               , dayofweek   = Saturday
               , dayofmonth  = 2
               }
        , Case { description = "first Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = First
               , dayofweek   = Sunday
               , dayofmonth  = 3
               }
        , Case { description = "first Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = First
               , dayofweek   = Sunday
               , dayofmonth  = 7
               }
        , Case { description = "second Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Monday
               , dayofmonth  = 11
               }
        , Case { description = "second Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Monday
               , dayofmonth  = 8
               }
        , Case { description = "second Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Second
               , dayofweek   = Tuesday
               , dayofmonth  = 14
               }
        , Case { description = "second Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Second
               , dayofweek   = Tuesday
               , dayofmonth  = 11
               }
        , Case { description = "second Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Second
               , dayofweek   = Wednesday
               , dayofmonth  = 10
               }
        , Case { description = "second Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Second
               , dayofweek   = Wednesday
               , dayofmonth  = 14
               }
        , Case { description = "second Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Second
               , dayofweek   = Thursday
               , dayofmonth  = 12
               }
        , Case { description = "second Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Second
               , dayofweek   = Thursday
               , dayofmonth  = 10
               }
        , Case { description = "second Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Second
               , dayofweek   = Friday
               , dayofmonth  = 8
               }
        , Case { description = "second Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Second
               , dayofweek   = Friday
               , dayofmonth  = 13
               }
        , Case { description = "second Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Second
               , dayofweek   = Saturday
               , dayofmonth  = 12
               }
        , Case { description = "second Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Second
               , dayofweek   = Saturday
               , dayofmonth  = 9
               }
        , Case { description = "second Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Second
               , dayofweek   = Sunday
               , dayofmonth  = 10
               }
        , Case { description = "second Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Second
               , dayofweek   = Sunday
               , dayofmonth  = 14
               }
        , Case { description = "third Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Monday
               , dayofmonth  = 18
               }
        , Case { description = "third Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Monday
               , dayofmonth  = 15
               }
        , Case { description = "third Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Third
               , dayofweek   = Tuesday
               , dayofmonth  = 21
               }
        , Case { description = "third Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Third
               , dayofweek   = Tuesday
               , dayofmonth  = 18
               }
        , Case { description = "third Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Third
               , dayofweek   = Wednesday
               , dayofmonth  = 17
               }
        , Case { description = "third Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Third
               , dayofweek   = Wednesday
               , dayofmonth  = 21
               }
        , Case { description = "third Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Third
               , dayofweek   = Thursday
               , dayofmonth  = 19
               }
        , Case { description = "third Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Third
               , dayofweek   = Thursday
               , dayofmonth  = 17
               }
        , Case { description = "third Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Third
               , dayofweek   = Friday
               , dayofmonth  = 15
               }
        , Case { description = "third Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Third
               , dayofweek   = Friday
               , dayofmonth  = 20
               }
        , Case { description = "third Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Third
               , dayofweek   = Saturday
               , dayofmonth  = 19
               }
        , Case { description = "third Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Third
               , dayofweek   = Saturday
               , dayofmonth  = 16
               }
        , Case { description = "third Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Third
               , dayofweek   = Sunday
               , dayofmonth  = 17
               }
        , Case { description = "third Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Third
               , dayofweek   = Sunday
               , dayofmonth  = 21
               }
        , Case { description = "fourth Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Monday
               , dayofmonth  = 25
               }
        , Case { description = "fourth Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Monday
               , dayofmonth  = 22
               }
        , Case { description = "fourth Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Fourth
               , dayofweek   = Tuesday
               , dayofmonth  = 28
               }
        , Case { description = "fourth Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Fourth
               , dayofweek   = Tuesday
               , dayofmonth  = 25
               }
        , Case { description = "fourth Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Fourth
               , dayofweek   = Wednesday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Fourth
               , dayofweek   = Wednesday
               , dayofmonth  = 28
               }
        , Case { description = "fourth Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Fourth
               , dayofweek   = Thursday
               , dayofmonth  = 26
               }
        , Case { description = "fourth Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Fourth
               , dayofweek   = Thursday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Fourth
               , dayofweek   = Friday
               , dayofmonth  = 22
               }
        , Case { description = "fourth Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Fourth
               , dayofweek   = Friday
               , dayofmonth  = 27
               }
        , Case { description = "fourth Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Fourth
               , dayofweek   = Saturday
               , dayofmonth  = 26
               }
        , Case { description = "fourth Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Fourth
               , dayofweek   = Saturday
               , dayofmonth  = 23
               }
        , Case { description = "fourth Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Fourth
               , dayofweek   = Sunday
               , dayofmonth  = 24
               }
        , Case { description = "fourth Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Fourth
               , dayofweek   = Sunday
               , dayofmonth  = 28
               }
        , Case { description = "last Monday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Monday
               , dayofmonth  = 25
               }
        , Case { description = "last Monday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Monday
               , dayofmonth  = 29
               }
        , Case { description = "last Tuesday of May 2013"
               , year        = 2013
               , month       = 5
               , week        = Last
               , dayofweek   = Tuesday
               , dayofmonth  = 28
               }
        , Case { description = "last Tuesday of June 2013"
               , year        = 2013
               , month       = 6
               , week        = Last
               , dayofweek   = Tuesday
               , dayofmonth  = 25
               }
        , Case { description = "last Wednesday of July 2013"
               , year        = 2013
               , month       = 7
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 31
               }
        , Case { description = "last Wednesday of August 2013"
               , year        = 2013
               , month       = 8
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 28
               }
        , Case { description = "last Thursday of September 2013"
               , year        = 2013
               , month       = 9
               , week        = Last
               , dayofweek   = Thursday
               , dayofmonth  = 26
               }
        , Case { description = "last Thursday of October 2013"
               , year        = 2013
               , month       = 10
               , week        = Last
               , dayofweek   = Thursday
               , dayofmonth  = 31
               }
        , Case { description = "last Friday of November 2013"
               , year        = 2013
               , month       = 11
               , week        = Last
               , dayofweek   = Friday
               , dayofmonth  = 29
               }
        , Case { description = "last Friday of December 2013"
               , year        = 2013
               , month       = 12
               , week        = Last
               , dayofweek   = Friday
               , dayofmonth  = 27
               }
        , Case { description = "last Saturday of January 2013"
               , year        = 2013
               , month       = 1
               , week        = Last
               , dayofweek   = Saturday
               , dayofmonth  = 26
               }
        , Case { description = "last Saturday of February 2013"
               , year        = 2013
               , month       = 2
               , week        = Last
               , dayofweek   = Saturday
               , dayofmonth  = 23
               }
        , Case { description = "last Sunday of March 2013"
               , year        = 2013
               , month       = 3
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 31
               }
        , Case { description = "last Sunday of April 2013"
               , year        = 2013
               , month       = 4
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 28
               }
        , Case { description = "last Wednesday of February 2012"
               , year        = 2012
               , month       = 2
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 29
               }
        , Case { description = "last Wednesday of December 2014"
               , year        = 2014
               , month       = 12
               , week        = Last
               , dayofweek   = Wednesday
               , dayofmonth  = 31
               }
        , Case { description = "last Sunday of February 2015"
               , year        = 2015
               , month       = 2
               , week        = Last
               , dayofweek   = Sunday
               , dayofmonth  = 22
               }
        , Case { description = "first Friday of December 2012"
               , year        = 2012
               , month       = 12
               , week        = First
               , dayofweek   = Friday
               , dayofmonth  = 7
               }
        ]

-- Food Chain
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "food-chain" $
--
--          describe "song" $ do
--
--            -- First we test the input, line by line, to give more
--            -- useful error messages.
--
--            it "matches lines" $ sequence_ lineAssertions
--
--            -- Finally, because testing lines we are unable
--            -- to detect a missing newline at the end of the
--            -- lyrics, we test the full song.
--
--            it "matches full song" $ song `shouldBe` lyrics
--  where
--
--    lineAssertions = zipWith checkLine [1 :: Int ..] $ zipMaybe (lines song) (lines lyrics)
--
--    checkLine lineno (got, want) =
--      unless (got == want) $
--        expectationFailure $ "mismatch at line " ++ show lineno ++ "\nexpected: " ++ show want ++ "\n but got: " ++ show got
--
--    zipMaybe    []     []  = []
--    zipMaybe (x:xs)    []  = (Just x , Nothing) : zipMaybe xs []
--    zipMaybe    []  (y:ys) = (Nothing, Just y ) : zipMaybe [] ys
--    zipMaybe (x:xs) (y:ys) = (Just x , Just y ) : zipMaybe xs ys
--
---- Lyrics extracted from `exercism/x-common` on 2016-09-21.
--
--lyrics :: String
--lyrics =
--    "I know an old lady who swallowed a fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a spider.\n\
--    \It wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a bird.\n\
--    \How absurd to swallow a bird!\n\
--    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a cat.\n\
--    \Imagine that, to swallow a cat!\n\
--    \She swallowed the cat to catch the bird.\n\
--    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a dog.\n\
--    \What a hog, to swallow a dog!\n\
--    \She swallowed the dog to catch the cat.\n\
--    \She swallowed the cat to catch the bird.\n\
--    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a goat.\n\
--    \Just opened her throat and swallowed a goat!\n\
--    \She swallowed the goat to catch the dog.\n\
--    \She swallowed the dog to catch the cat.\n\
--    \She swallowed the cat to catch the bird.\n\
--    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a cow.\n\
--    \I don't know how she swallowed a cow!\n\
--    \She swallowed the cow to catch the goat.\n\
--    \She swallowed the goat to catch the dog.\n\
--    \She swallowed the dog to catch the cat.\n\
--    \She swallowed the cat to catch the bird.\n\
--    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
--    \She swallowed the spider to catch the fly.\n\
--    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
--    \\n\
--    \I know an old lady who swallowed a horse.\n\
--    \She's dead, of course!\n"
-- OCR
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "ocr-numbers" $
--          describe "convert" $ for_ cases test
--  where
--
--    test Case{..} = it description assertion
--      where
--        assertion = convert (unlines input) `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common/ocr-numbers.json`
---- on 2016-08-09. Some deviations exist and are noted in comments.
--
--data Case = Case { description ::  String
--                 , expected    ::  String
--                 , input       :: [String]
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "Recognizes 0"
--               , expected    = "0"
--               , input       = [ " _ "
--                               , "| |"
--                               , "|_|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 1"
--               , expected    = "1"
--               , input       = [ "   "
--                               , "  |"
--                               , "  |"
--                               , "   " ]
--               }
--        , Case { description = "Unreadable but correctly sized inputs return ?"
--               , expected    = "?"
--               , input       = [ "   "
--                               , "  _"
--                               , "  |"
--                               , "   " ]
--               }
--
--        {- In this track, the tests to determine if the input
--           has the correct format where not implemented.
--
--        , Case { description = "Input with a number of lines that is not a multiple of four raises an error"
--               , expected    = -1
--               , input       = [ " _ "
--                               , "| |"
--                               , "   " ]
--               }
--        , Case { description = "Input with a number of columns that is not a multiple of three raises an error"
--               , expected    = -1
--               , input       = [ "    "
--                               , "   |"
--                               , "   |"
--                               , "    " ]
--               }
--        -}
--
--        , Case { description = "Recognizes 110101100"
--               , expected    = "110101100"
--               , input       = [ "       _     _        _  _ "
--                               , "  |  || |  || |  |  || || |"
--                               , "  |  ||_|  ||_|  |  ||_||_|"
--                               , "                           " ]
--               }
--        , Case { description = "Garbled numbers in a string are replaced with ?"
--               , expected    = "11?10?1?0"
--               , input       = [ "       _     _           _ "
--                               , "  |  || |  || |     || || |"
--                               , "  |  | _|  ||_|  |  ||_||_|"
--                               , "                           " ]
--               }
--        , Case { description = "Recognizes 2"
--               , expected    = "2"
--               , input       = [ " _ "
--                               , " _|"
--                               , "|_ "
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 3"
--               , expected    = "3"
--               , input       = [ " _ "
--                               , " _|"
--                               , " _|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 4"
--               , expected    = "4"
--               , input       = [ "   "
--                               , "|_|"
--                               , "  |"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 5"
--               , expected    = "5"
--               , input       = [ " _ "
--                               , "|_ "
--                               , " _|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 6"
--               , expected    = "6"
--               , input       = [ " _ "
--                               , "|_ "
--                               , "|_|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 7"
--               , expected    = "7"
--               , input       = [ " _ "
--                               , "  |"
--                               , "  |"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 8"
--               , expected    = "8"
--               , input       = [ " _ "
--                               , "|_|"
--                               , "|_|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes 9"
--               , expected    = "9"
--               , input       = [ " _ "
--                               , "|_|"
--                               , " _|"
--                               , "   " ]
--               }
--        , Case { description = "Recognizes string of decimal numbers"
--               , expected    = "1234567890"
--               , input       = [ "    _  _     _  _  _  _  _  _ "
--                               , "  | _| _||_||_ |_   ||_||_|| |"
--                               , "  ||_  _|  | _||_|  ||_| _||_|"
--                               , "                              " ]
--               }
--        , Case { description = "Numbers separated by empty lines are recognized. Lines are joined by commas."
--               , expected    = "123,456,789"
--               , input       = [ "    _  _ "
--                               , "  | _| _|"
--                               , "  ||_  _|"
--                               , "         "
--                               , "    _  _ "
--                               , "|_||_ |_ "
--                               , "  | _||_|"
--                               , "         "
--                               , " _  _  _ "
--                               , "  ||_||_|"
--                               , "  ||_| _|"
--                               , "         " ]
--               }
--        ]
--
-- Pascal's triangle
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "pascals-triangle" $
--          describe "rows" $ for_ rowsCases rowsTest
--  where
--    rowsTest (description, n, expected) = it description assertion
--      where
--        assertion = rows n `shouldBe` expected
--    -- Test cases adapted from `exercism/x-common` on 2016-09-14.
--    rowsCases = [ ("no rows"      , 0, [                                    ])
--                , ("single row"   , 1, [[1]                                 ])
--                , ("two rows"     , 2, [[1], [1, 1]                         ])
--                , ("three rows"   , 3, [[1], [1, 1], [1, 2, 1]              ])
--                , ("four rows"    , 4, [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]])
--                , ("negative rows",-1, [                                    ]) ]
--
-- Pythagorean triplets
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "pythagorean-triplet" $ do
--          describe "isPythagorean"       $ for_ isPythagoreanCases       isPythagoreanTest
--          describe "pythagoreanTriplets" $ for_ pythagoreanTripletsCases pythagoreanTripletsTest
--  where
--
--    isPythagoreanTest ((a, b, c), expected) = it description assertion
--      where
--        description = unwords $ show <$> [a, b, c]
--        assertion   = isPythagorean (mkTriplet a b c) `shouldBe` expected
--
--    pythagoreanTripletsTest (x, y, ts) = it description assertion
--      where
--        description = unwords $ show <$> [x, y]
--        assertion   = pythagoreanTriplets x y `shouldBe` uncurry3 mkTriplet <$> ts
--
--    uncurry3 f (x, y, z) = f x y z
--
--    -- As of 2016-09-12, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    isPythagoreanCases = [ ( (3, 4, 5), True )
--                         , ( (3, 5, 4), True )
--                         , ( (4, 3, 5), True )
--                         , ( (4, 5, 3), True )
--                         , ( (5, 3, 4), True )
--                         , ( (5, 4, 3), True )
--                         , ( (3, 3, 3), False)
--                         , ( (5, 6, 7), False) ]
--
--    pythagoreanTripletsCases = [ (1 , 10, [ ( 3,  4,  5), ( 6,  8, 10) ])
--                               , (11, 20, [ (12, 16, 20)               ])
--                               , (56, 95, [ (57, 76, 95), (60, 63, 87) ]) ]
----
---- Primes
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "nth-prime" $
--          describe "nth" $ for_ cases test
--  where
--
--    test Case{..} = it description assertion
--      where
--        assertion = nth (fromIntegral input) `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common` on 2016-09-19.
--
--data Case = Case { description :: String
--                 , input       :: Integer
--                 , expected    :: Maybe Integer
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "first prime"
--               , input       = 1
--               , expected    = Just 2
--               }
--        , Case { description = "second prime"
--               , input       = 2
--               , expected    = Just 3
--               }
--        , Case { description = "sixth prime"
--               , input       = 6
--               , expected    = Just 13
--               }
--        , Case { description = "big prime"
--               , input       = 10001
--               , expected    = Just 104743
--               }
--        , Case { description = "there is no zeroth prime"
--               , input       = 0
--               , expected    = Nothing
--               }
--        ]
---- Allergies
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "allergies" $ do
--
--          -- Test cases adapted from `exercism/x-common/allergies.json` on 2016-08-01.
--
--          describe "isAllergicTo" $ do
--
--            it "no allergies means not allergic" $ do
--              let score = 0
--              isAllergicTo Peanuts      score `shouldBe` False
--              isAllergicTo Cats         score `shouldBe` False
--              isAllergicTo Strawberries score `shouldBe` False
--
--            it "is allergic to eggs" $ do
--              let score = 1
--              isAllergicTo Eggs         score `shouldBe` True
--
--            it "allergic to eggs in addition to other stuff" $ do
--              let score = 5
--              isAllergicTo Eggs         score `shouldBe` True
--              isAllergicTo Shellfish    score `shouldBe` True
--              isAllergicTo Strawberries score `shouldBe` False
--
--          describe "allergies" $ do
--
--            let xs `shouldMatch` ys =  all (`elem` ys) xs
--                                    && all (`elem` xs) ys
--
--            it "no allergies at all" $
--              allergies   0 `shouldMatch` []
--
--            it "allergic to just eggs" $
--              allergies   1 `shouldMatch` [ Eggs ]
--
--            it "allergic to just peanuts" $
--              allergies   2 `shouldMatch` [ Peanuts ]
--
--            it "allergic to just strawberries" $
--              allergies   8 `shouldMatch` [ Strawberries ]
--
--            it "allergic to eggs and peanuts" $
--              allergies   3 `shouldMatch` [ Eggs
--                                          , Peanuts ]
--
--            it "allergic to more than eggs but not peanuts" $
--              allergies   5 `shouldMatch` [ Eggs
--                                          , Shellfish ]
--
--            it "allergic to lots of stuff" $
--              allergies 248 `shouldMatch` [ Cats
--                                          , Chocolate
--                                          , Pollen
--                                          , Strawberries
--                                          , Tomatoes     ]
--
--            it "allergic to everything" $
--              allergies 255 `shouldMatch` [ Cats
--                                          , Chocolate
--                                          , Eggs
--                                          , Peanuts
--                                          , Pollen
--                                          , Shellfish
--                                          , Strawberries
--                                          , Tomatoes     ]
--
--            it "ignore non allergen score parts" $
--              allergies 509 `shouldMatch` [ Cats
--                                          , Chocolate
--                                          , Eggs
--                                          , Pollen
--                                          , Shellfish
--                                          , Strawberries
--                                          , Tomatoes     ]
--
------ Largest Series Product
----main :: IO ()
----main = hspecWith defaultConfig {configFastFail = True} specs
----
----specs :: Spec
----specs = describe "largest-series-product" $
----
----    -- Test cases adapted from `exercism/x-common/largest-series-product.json`
----    -- on 2016-07-27.
----
----    describe "largestProduct" $ do
----
----      it "can find the largest product of 2 with numbers in order" $
----        largestProduct 2 "0123456789"
----        `shouldBe` Just 72
----
----      it "can find the largest product of 2" $
----        largestProduct 2 "576802143"
----        `shouldBe` Just 48
----
----      it "finds the largest product if span equals length" $
----        largestProduct 2 "29"
----        `shouldBe` Just 18
----
----      it "can find the largest product of 3 with numbers in order" $
----        largestProduct 3 "0123456789"
----        `shouldBe` Just 504
----
----      it "can find the largest product of 3" $
----        largestProduct 3 "1027839564"
----        `shouldBe` Just 270
----
----      it "can find the largest product of 5 with numbers in order" $
----        largestProduct 5 "0123456789"
----        `shouldBe` Just 15120
----
----      it "can get the largest product of a big number" $
----        largestProduct 6 "73167176531330624919225119674426574742355349194934"
----        `shouldBe` Just 23520
----
----      it "can get the largest product of a big number II" $
----        largestProduct 6 "52677741234314237566414902593461595376319419139427"
----        `shouldBe` Just 28350
----
----      it "can get the largest product of a big number (Project Euler)" $
----        largestProduct 13 "73167176531330624919225119674426574742355349194934\
----                          \96983520312774506326239578318016984801869478851843\
----                          \85861560789112949495459501737958331952853208805511\
----                          \12540698747158523863050715693290963295227443043557\
----                          \66896648950445244523161731856403098711121722383113\
----                          \62229893423380308135336276614282806444486645238749\
----                          \30358907296290491560440772390713810515859307960866\
----                          \70172427121883998797908792274921901699720888093776\
----                          \65727333001053367881220235421809751254540594752243\
----                          \52584907711670556013604839586446706324415722155397\
----                          \53697817977846174064955149290862569321978468622482\
----                          \83972241375657056057490261407972968652414535100474\
----                          \82166370484403199890008895243450658541227588666881\
----                          \16427171479924442928230863465674813919123162824586\
----                          \17866458359124566529476545682848912883142607690042\
----                          \24219022671055626321111109370544217506941658960408\
----                          \07198403850962455444362981230987879927244284909188\
----                          \84580156166097919133875499200524063689912560717606\
----                          \05886116467109405077541002256983155200055935729725\
----                          \71636269561882670428252483600823257530420752963450"
----        `shouldBe` Just 23514624000
----
----      it "reports zero if the only digits are zero" $
----        largestProduct 2 "0000"
----        `shouldBe` Just 0
----
----      it "reports zero if all spans include zero" $
----        largestProduct 3 "99099"
----        `shouldBe` Just 0
----
----      it "rejects span longer than string length" $
----        largestProduct 4 "123"
----        `shouldBe` Nothing
----
----      it "reports 1 for empty string and empty product (0 span)" $
----        largestProduct 0 ""
----        `shouldBe` Just 1
----
----      it "reports 1 for nonempty string and empty product (0 span)" $
----        largestProduct 0 "123"
----        `shouldBe` Just 1
----
----      it "rejects empty string and nonzero span" $
----        largestProduct 1 ""
----        `shouldBe` Nothing
----
----      it "rejects invalid character in digits" $
----        largestProduct 2 "1234a5"
----        `shouldBe` Nothing
----
----      it "rejects negative span" $
----        largestProduct (-1) "12345"
----        `shouldBe` Nothing
--
---- Prime factors
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "prime-factors" $
--          describe "primeFactors" $ for_ cases test
--  where
--
--    test (n, expected) = it explanation assertion
--      where
--        explanation = show n
--        assertion   = primeFactors n `shouldBe` expected
--
--    -- As of 2016-07-31, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    cases = [ (          1,                 [] )
--            , (          2,                [2] )
--            , (          3,                [3] )
--            , (          4,             [2, 2] )
--            , (          6,             [2, 3] )
--            , (          8,          [2, 2, 2] )
--            , (          9,             [3, 3] )
--            , (         27,          [3, 3, 3] )
--            , (        625,       [5, 5, 5, 5] )
--            , (     901255,   [5, 17, 23, 461] )
--            , (93819012551, [11, 9539, 894119] ) ]
--

----Raindrops
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "raindrops" $
--          describe "convert" $ for_ cases test
--  where
--    test (number, expected) = it description assertion
--      where
--        description = show number
--        assertion   = convert number `shouldBe` expected
--
--    -- Test cases adapted from `exercism/x-common` on 2016-09-19.
--
--    cases = [ (   1, "1"              )
--            , (   3, "Pling"          )
--            , (   5, "Plang"          )
--            , (   7, "Plong"          )
--            , (   6, "Pling"          )
--            , (   8, "8"              )
--            , (   9, "Pling"          )
--            , (  10, "Plang"          )
--            , (  14, "Plong"          )
--            , (  15, "PlingPlang"     )
--            , (  21, "PlingPlong"     )
--            , (  25, "Plang"          )
--            , (  27, "Pling"          )
--            , (  35, "PlangPlong"     )
--            , (  49, "Plong"          )
--            , (  52, "52"             )
--            , ( 105, "PlingPlangPlong")
--            , (3125, "Plang"          ) ]
--
--
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "triangle" $
--          describe "triangleType" $ for_ cases test
--  where
--
--    test (description, (a, b, c), expected) = it description assertion
--      where
--        assertion = triangleType a b c `shouldBe` expected
--
--    -- Test cases adapted from `exercism/x-common/triangle.json` on 2016-08-03.
--
--    cases = [ ( "equilateral triangle has all sides equal"
--              , (2, 2, 2)
--              , Equilateral
--              )
--            , ( "larger equilateral triangle"
--              , (10, 10, 10)
--              , Equilateral
--              )
--            , ( "isosceles triangle with last two sides equal"
--              , (3, 4, 4)
--              , Isosceles
--              )
--            , ( "isosceles triangle with first two sides equal"
--              , (4, 4, 3)
--              , Isosceles
--              )
--            , ( "isosceles triangle with first and last sides equal"
--              , (4, 3, 4)
--              , Isosceles
--              )
--            , ( "isosceles triangle with unequal side larger than equal sides"
--              , (4, 7, 4)
--              , Isosceles
--              )
--            , ( "scalene triangle has no equal sides"
--              , (3, 4, 5)
--              , Scalene
--              )
--            , ( "larger scalene triangle"
--              , (10, 11, 12)
--              , Scalene
--              )
--            , ( "scalene triangle with sides in descending order"
--              , (5, 4, 2)
--              , Scalene
--              )
--            , ( "small scalene triangle with floating point values"
--              , (0.4, 0.6, 0.3)
--              , Scalene
--              )
--            , ( "a triangle violating the triangle inequality is illegal"
--              , (7, 3, 2)
--              , Illegal
--              )
--            , ( "two sides equal, but still violates triangle inequality"
--              , (1, 1, 3)
--              , Illegal
--              )
--            , ( "triangles with all sides zero are illegal"
--              , (0, 0, 0)
--              , Illegal
--              )
--            ]
--
---- Scrabble
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "scrabble-score" $ do
--          describe "scoreLetter" $ do
--            it "'a'" $ scoreLetter 'a' `shouldBe`  1
--            it "'Z'" $ scoreLetter 'Z' `shouldBe` 10
--            it "'?'" $ scoreLetter '?' `shouldBe`  0
--          describe "scoreWord" $ for_ cases test
--  where
--
--    test Case{..} = it description assertion
--      where
--        assertion = scoreWord input `shouldBe` fromIntegral expected
--
---- Test cases adapted from `exercism/x-common/scrabble-score.json` on 2016-07-26.
--
--data Case = Case { description :: String
--                 , input       :: String
--                 , expected    :: Integer
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "lowercase letter"
--               , input       = "a"
--               , expected    = 1
--               }
--        , Case { description = "uppercase letter"
--               , input       = "A"
--               , expected    = 1
--               }
--        , Case { description = "valuable letter"
--               , input       = "f"
--               , expected    = 4
--               }
--        , Case { description = "short word"
--               , input       = "at"
--               , expected    = 2
--               }
--        , Case { description = "short, valuable word"
--               , input       = "zoo"
--               , expected    = 12
--               }
--        , Case { description = "medium word"
--               , input       = "street"
--               , expected    = 6
--               }
--        , Case { description = "medium, valuable word"
--               , input       = "quirky"
--               , expected    = 22
--               }
--        , Case { description = "long, mixed-case word"
--               , input       = "OxyphenButazone"
--               , expected    = 41
--               }
--        , Case { description = "english-like word"
--               , input       = "pinata"
--               , expected    = 8
--               }
--        , Case { description = "non-english letter is not scored"
--               , input       = "piñata"
--               , expected    = 7
--               }
--        , Case { description = "empty input"
--               , input       = ""
--               , expected    = 0
--               }
--        ]
--
---- ETL
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "etl" $
--
--  -- As of 2016-07-27, there was no reference file
--  -- for the test cases in `exercism/x-common`.
--
--  describe "transform" $ do
--
--    it "transform one value" $
--      transform (fromList [(1, "A")])
--      `shouldBe` fromList [('a', 1)]
--
--    it "transform multiple keys from one value" $
--      transform (fromList [(1, "AE")])
--      `shouldBe` fromList [('a', 1), ('e', 1)]
--
--    it "transform multiple keys from multiple values" $
--      transform (fromList [(1, "A"), (4, "B")])
--      `shouldBe` fromList [('a', 1), ('b', 4)]
--
--    it "full dataset" $
--      transform (fromList fullInput)
--      `shouldBe` fromList fullOutput
--
--  where
--
--    fullInput = [ ( 1, "AEIOULNRST")
--                , ( 2, "DG"        )
--                , ( 3, "BCMP"      )
--                , ( 4, "FHVWY"     )
--                , ( 5, "K"         )
--                , ( 8, "JX"        )
--                , (10, "QZ"        ) ]
--
--    fullOutput = [ ('a',  1) , ('b',  3) , ('c',  3) , ('d',  2)
--                 , ('e',  1) , ('f',  4) , ('g',  2) , ('h',  4)
--                 , ('i',  1) , ('j',  8) , ('k',  5) , ('l',  1)
--                 , ('m',  3) , ('n',  1) , ('o',  1) , ('p',  3)
--                 , ('q', 10) , ('r',  1) , ('s',  1) , ('t',  1)
--                 , ('u',  1) , ('v',  4) , ('w',  4) , ('x',  8)
--                 , ('y',  4) , ('z', 10) ]
--
---- ListOps
--data StrictException = StrictException deriving (Eq, Show, Exception)
--
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "list-ops" $ do
--
--    -- As of 2016-07-27, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    let big = 100000 :: Int
--
--    it "length of empty list" $
--      length ([] :: [Int]) `shouldBe` 0
--
--    it "length of non-empty list" $
--      length [1 .. 4 :: Int] `shouldBe` 4
--
--    it "length of large list" $
--      length [1 .. big :: Int] `shouldBe` big
--
--    it "reverse of empty list" $
--      reverse ([] :: [Int]) `shouldBe` []
--
--    it "reverse of non-empty list" $
--      reverse [1 .. 100 :: Int] `shouldBe` [100 , 99 .. 1]
--
--    it "map of empty list" $
--      map (+1) ([] :: [Int]) `shouldBe` []
--
--    it "map of non-empty list" $
--      map (+1) [1, 3 .. 7 :: Int] `shouldBe` [2, 4 .. 8]
--
--    it "filter of empty list" $
--      filter undefined ([] :: [Int]) `shouldBe` []
--
--    it "filter of normal list" $
--      filter odd [1 .. 4 :: Int] `shouldBe` [1, 3]
--
--    it "foldl' of empty list" $
--      foldl' (+) (0 :: Int) [] `shouldBe` 0
--
--    it "foldl' of non-empty list" $
--      foldl' (+) (-3) [1 .. 4 :: Int] `shouldBe` 7
--
--    it "foldl' of huge list" $
--      foldl' (+) 0 [1 .. big] `shouldBe` big * (big + 1) `div` 2
--
--    it "foldl' with non-commutative function" $
--      foldl' (-) 10 [1 .. 4 :: Int] `shouldBe` 0
--
--    it "foldl' is not just foldr . flip" $
--      foldl' (flip (:)) [] "asdf" `shouldBe` "fdsa"
--
--    it "foldl' is accumulator-strict (use seq or BangPatterns)" $
--      evaluate (foldl' (flip const) () [throw StrictException, ()])
--      `shouldThrow` (== StrictException)
--
--    it "foldr as id" $
--      foldr (:) [] [1 .. big] `shouldBe` [1 .. big]
--
--    it "foldr as append" $
--      foldr (:) [100 .. big] [1 .. 99] `shouldBe` [1 .. big]
--
--    it "++ of empty lists" $
--      [] ++ ([] :: [Int]) `shouldBe` []
--
--    it "++ of empty and non-empty lists" $
--      [] ++ [1 .. 4 :: Int] `shouldBe` [1 .. 4]
--
--    it "++ of non-empty and empty lists" $
--      [1 .. 4 :: Int] ++ [] `shouldBe` [1 .. 4]
--
--    it "++ of non-empty lists" $
--      [1 .. 3] ++ [4, 5 :: Int] `shouldBe` [1 .. 5]
--
--    it "++ of large lists" $
--      [1 .. big `div` 2] ++ [1 + big `div` 2 .. big] `shouldBe` [1 .. big]
--
--    it "concat of no lists" $
--      concat ([] :: [[Int]]) `shouldBe` []
--
--    it "concat of list of lists" $
--      concat [[1, 2], [3], [], [4, 5, 6 :: Int]] `shouldBe` [1 .. 6]
--
--    it "concat of large list of small lists" $
--      concat (map (:[]) [1 .. big]) `shouldBe` [1 .. big]
--
-- LinkedList
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "simple-linked-list" $ do
--
--            -- As of 2016-07-27, there was no reference file
--            -- for the test cases in `exercism/x-common`.
--
--            let n1   = new (1 :: Int) nil
--            let n21  = new 2 n1
--            let n321 = new 3 n21
--            let fl1  = fromList [1 :: Int]
--            let fl21 = fromList [2, 1 :: Int]
--            let r1   = reverseLinkedList n1
--            let r12  = reverseLinkedList n21
--            let r123 = reverseLinkedList n321
--            let msg  = "Should work for any type, not just Int!"
--
--            it "constructor" $ do
--                isNil nil               `shouldBe` True
--                isNil n1                `shouldBe` False
--                datum n1                `shouldBe` 1
--                isNil (next n1)         `shouldBe` True
--                isNil n21               `shouldBe` False
--                datum n21               `shouldBe` 2
--                isNil (next n21)        `shouldBe` False
--                datum (next n21)        `shouldBe` 1
--                isNil (next $ next n21) `shouldBe` True
--
--            it "toList" $ do
--                toList nil `shouldBe` ([] :: [Int])
--                toList n1  `shouldBe` [1]
--                toList n21 `shouldBe` [2, 1]
--
--            it "fromList" $ do
--                isNil (fromList [])      `shouldBe` True
--                datum fl1                `shouldBe` 1
--                isNil (next fl1)         `shouldBe` True
--                datum fl21               `shouldBe` 2
--                datum (next fl21)        `shouldBe` 1
--                isNil (next $ next fl21) `shouldBe` True
--
--            it "reverseList" $ do
--                isNil (reverseLinkedList nil) `shouldBe` True
--                datum r1                      `shouldBe` 1
--                isNil (next r1)               `shouldBe` True
--                datum r12                     `shouldBe` 1
--                datum (next r12)              `shouldBe` 2
--                isNil (next $ next r12)       `shouldBe` True
--                datum r123                    `shouldBe` 1
--                datum (next r123)             `shouldBe` 2
--                datum (next $ next r123)      `shouldBe` 3
--
--            it "roundtrip" $ do
--                (toList . fromList) []      `shouldBe` ([]      :: [Int])
--                (toList . fromList) [1]     `shouldBe` ([1]     :: [Int])
--                (toList . fromList) [1, 2]  `shouldBe` ([1, 2]  :: [Int])
--                (toList . fromList) [1..10] `shouldBe` ([1..10] :: [Int])
--
--            it "has an unconstrained type variable" $ do
--                (toList . fromList) msg     `shouldBe` msg
--                (toList . fromList) [1..10] `shouldBe` ([1..10] :: [Integer])
--
---- Grade School
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "grade-school" $ do
--
--          -- As of 2016-07-27, there was no reference file
--          -- for the test cases in `exercism/x-common`.
--
--          let fromList = foldr (uncurry add) empty
--          let fromGrade g = fromList . zip (repeat g)
--
--          it "add student" $
--            sorted (add 2 "Aimee" empty) `shouldBe` [(2, ["Aimee"])]
--
--          it "add more students in same class" $
--            sorted (fromGrade 2 ["James", "Blair", "Paul"])
--            `shouldBe` [(2, ["Blair", "James", "Paul"])]
--
--          it "add students to different grades" $
--            sorted (fromList [(3, "Chelsea"), (7, "Logan")])
--            `shouldBe` [(3, ["Chelsea"]), (7, ["Logan"])]
--
--          it "get students in a grade" $
--            grade 5 (fromList [(5, "Franklin"), (5, "Bradley"), (1, "Jeff")])
--            `shouldBe` ["Bradley", "Franklin"]
--
--          it "get students in a non-existent grade" $
--            grade 1 empty `shouldBe` []
--
--          it "sorted school" $
--            sorted (fromList [ (4, "Jennifer"   )
--                             , (6, "Kareem"     )
--                             , (4, "Christopher")
--                             , (3, "Kyle"       ) ] )
--            `shouldBe` [ (3, ["Kyle"                   ] )
--                       , (4, ["Christopher", "Jennifer"] )
--                       , (6, ["Kareem"                 ] ) ]
--
---- PhoneNumber
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "phone-number" $ do
--          describe "number"      $ for_ numberCases      $ test number
--          describe "areaCode"    $ for_ areaCodeCases    $ test areaCode
--          describe "prettyPrint" $ for_ prettyPrintCases $ test prettyPrint
--  where
--    test f Case{..} = it description $ f input `shouldBe` expected
--
---- As of 2016-07-27, there was no reference file
---- for the test cases in `exercism/x-common`.
--
--data Case = Case { description ::       String
--                 , input       ::       String
--                 , expected    :: Maybe String
--                 }
--
--numberCases :: [Case]
--numberCases =
--    [ Case { description = "cleans number"
--           , input       = "(123) 456-7890"
--           , expected    = Just "1234567890"
--           }
--    , Case { description = "cleans another number"
--           , input       = "(612) 555-1212"
--           , expected    = Just "6125551212"
--           }
--    , Case { description = "cleans number with dots"
--           , input       = "123.456.7890"
--           , expected    = Just "1234567890"
--           }
--    , Case { description = "cleans another number with dots"
--           , input       = "918.765.4321"
--           , expected    = Just "9187654321"
--           }
--    , Case { description = "valid when 11 digits and first is 1"
--           , input       = "12468013579"
--           , expected    = Just "2468013579"
--           }
--    , Case { description = "invalid when 11 digits"
--           , input       = "21234567890"
--           , expected    = Nothing
--           }
--    , Case { description = "invalid when 9 digits"
--           , input       = "123456789"
--           , expected    = Nothing
--           }
--    , Case { description = "invalid when 12 digits"
--           , input       = "123456789012"
--           , expected    = Nothing
--           }
--    , Case { description = "invalid when empty"
--           , input       = ""
--           , expected    = Nothing
--           }
--    , Case { description = "invalid when no digits present"
--           , input       = " (-) "
--           , expected    = Nothing
--           }
--    , Case { description = "valid with leading characters"
--           , input       = "my number is 235 813 2134"
--           , expected    = Just "2358132134"
--           }
--    , Case { description = "valid with trailing characters"
--           , input       = "987 654 3210 - bob"
--           , expected    = Just "9876543210"
--           }
--    , Case { description = "valid amidst text and punctuation"
--           , input       = "Here it is: 415-888-0000. Thanks!"
--           , expected    = Just "4158880000"
--           }
--    ]
--
--areaCodeCases :: [Case]
--areaCodeCases =
--    [ Case { description = "area code"
--           , input       = "1234567890"
--           , expected    = Just "123"
--           }
--    , Case { description = "area code with parentheses"
--           , input       = "(612) 555-1212"
--           , expected    = Just "612"
--           }
--    , Case { description = "area code with leading characters"
--           , input       = "my number is 235 813 2134"
--           , expected    = Just "235"
--           }
--    , Case { description = "invalid area code"
--           , input       = " (-) "
--           , expected    = Nothing
--           }
--    ]
--
--prettyPrintCases :: [Case]
--prettyPrintCases =
--    [ Case { description = "pretty print"
--           , input       = "1234567890"
--           , expected    = Just "(123) 456-7890"
--           }
--    , Case { description = "pretty print with full US phone number"
--           , input       = "12345678901"
--           , expected    = Just "(234) 567-8901"
--           }
--    , Case { description = "pretty print amidst text and punctuation"
--           , input       = "Here it is: 415-888-0000. Thanks!"
--           , expected    = Just "(415) 888-0000"
--           }
--    ]
--
--
---- Strain
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "strain" $ do
--
--    -- As of 2016-07-27, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    it "empty keep" $
--        keep (<10) [] `shouldBe` ([] :: [Int])
--
--    it "keep everything" $
--        keep (<10) [1, 2, 3] `shouldBe` [1, 2, 3 :: Int]
--
--    it "keep first and last" $
--        keep odd [1, 2, 3] `shouldBe` [1, 3 :: Int]
--
--    it "keep nothing" $
--        keep even [1, 3, 5, 7] `shouldBe` ([] :: [Int])
--
--    it "keep neither first nor last" $
--        keep even [1, 2, 3] `shouldBe` [2 :: Int]
--
--    it "keep strings" $
--        keep ("z" `isPrefixOf`)
--        ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
--        `shouldBe`
--        ["zebra", "zombies", "zealot"]
--
--    it "empty discard" $
--        discard (< 10) [] `shouldBe` ([] :: [Int])
--
--    it "discard everything" $
--        discard (< 10) [1, 2, 3] `shouldBe` ([] :: [Int])
--
--    it "discard first and last" $
--        discard odd [1, 2, 3] `shouldBe` [2 :: Int]
--
--    it "discard nothing" $
--        discard even [1, 3, 5, 7] `shouldBe` [1, 3, 5, 7 :: Int]
--
--    it "discard neither first nor last" $
--        discard even [1, 2, 3] `shouldBe` [1, 3 :: Int]
--
--    it "discard strings" $
--        discard ("z" `isPrefixOf`)
--        ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
--        `shouldBe`
--        ["apple", "banana", "cherimoya"]
--
--    it "keep non-strict" $
--        (take 1 . keep (const True))
--        ("yes" : error "keep should be lazier - don't look at list elements you don't need!")
--        `shouldBe`
--        ["yes"]
--
--    it "discard non-strict" $
--        (take 1 . discard (const False))
--        ("yes" : error "discard should be lazier - don't look at list elements you don't need!")
--        `shouldBe`
--        ["yes"]
--
---- Leap
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "leap" $
--          describe "isLeapYear" $ for_ cases test
--  where
--
--    test Case{..} = it explanation assertion
--      where
--        explanation = unwords [show input, "-", description]
--        assertion   = isLeapYear (fromIntegral input) `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common/leap.json` on 2016-07-27.
--
--data Case = Case { description :: String
--                 , input       :: Integer
--                 , expected    :: Bool
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "leap year"
--               , input       = 1996
--               , expected    = True
--               }
--        , Case { description = "standard and odd year"
--               , input       = 1997
--               , expected    = False
--               }
--        , Case { description = "standard even year"
--               , input       = 1998
--               , expected    = False
--               }
--        , Case { description = "standard nineteenth century"
--               , input       = 1900
--               , expected    = False
--               }
--        , Case { description = "standard eighteenth century"
--               , input       = 1800
--               , expected    = False
--               }
--        , Case { description = "leap twenty fourth century"
--               , input       = 2400
--               , expected    = True
--               }
--        , Case { description = "leap y2k"
--               , input       = 2000
--               , expected    = True
--               }
--        ]
--

---- RobotSimulator
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "robot-simulator" $ do
--
--    -- Test cases adapted from `exercism/x-common/robot-simulator.json`
--    -- on 2016-08-02. Some deviations exist and are noted in comments.
--
--    describe "mkRobot" $ do
--
--    -- The function described by the reference file
--    -- as `create` is called `mkRobot` in this track.
--
--      it "A robot is created with a position and a direction" $ do
--        let robot = mkRobot North (0, 0)
--        coordinates robot `shouldBe` (0, 0)
--        bearing     robot `shouldBe` North
--
--      it "Negative positions are allowed" $ do
--        let robot = mkRobot South (-1, -1)
--        coordinates robot `shouldBe` (-1, -1)
--        bearing     robot `shouldBe` South
--
--    -- The reference tests for `turnLeft` and `turnRight` describe
--    -- functions that are applied to robots positioned at (0, 0).
--    -- In this track, they are functions over directions, so those
--    -- test cases cannot be completely implemented.
--
--    describe "turnRight" $ do
--
--      it "turn from North" $ turnRight North `shouldBe` East
--      it "turn from East"  $ turnRight East  `shouldBe` South
--      it "turn from South" $ turnRight South `shouldBe` West
--      it "turn from West"  $ turnRight West  `shouldBe` North
--
--    describe "turnLeft" $ do
--
--      it "turn from North" $ turnLeft North `shouldBe` West
--      it "turn from West"  $ turnLeft West  `shouldBe` South
--      it "turn from South" $ turnLeft South `shouldBe` East
--      it "turn from East"  $ turnLeft East  `shouldBe` North
--
--    describe "simulate advance" $ do
--
--    -- The function described by the reference file as `advance`
--    -- doesn't exist in this track, so we test `simulate` with "A".
--
--      let dir `from` pos = simulate (mkRobot dir pos) "A"
--
--      it "does not change the direction" $
--        bearing (North `from` (0, 0)) `shouldBe` North
--
--      it "increases the y coordinate one when facing north" $
--        coordinates (North `from` (0, 0)) `shouldBe` (0, 1)
--
--      it "decreases the y coordinate by one when facing south" $
--        coordinates (South `from` (0, 0)) `shouldBe` (0, -1)
--
--      it "increases the x coordinate by one when facing east" $
--        coordinates (East `from` (0, 0)) `shouldBe` (1, 0)
--
--      it "decreases the x coordinate by one when facing west" $
--        coordinates (West `from` (0, 0)) `shouldBe `(-1, 0)
--
--    describe "simulate" $ do
--
--    -- The function described by the reference file as
--    -- `instructions` is called `simulate` in this track.
--
--      let simulation pos dir = simulate (mkRobot dir pos)
--
--      it "instructions to move west and north" $ do
--        let robot = simulation (0, 0) North "LAAARALA"
--        coordinates robot `shouldBe` (-4, 1)
--        bearing     robot `shouldBe` West
--
--      it "instructions to move west and south" $ do
--        let robot = simulation (2, -7) East "RRAAAAALA"
--        coordinates robot `shouldBe` (-3, -8)
--        bearing     robot `shouldBe` South
--
--      it "instructions to move east and north" $ do
--        let robot = simulation (8, 4) South "LAAARRRALLLL"
--        coordinates robot `shouldBe` (11, 5)
--        bearing     robot `shouldBe` North
--
---- Wordy
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "wordy" $
--          describe "answer" $ for_ cases test
--  where
--
--    test Case{..} = it description assertion
--      where
--        assertion   = answer input `shouldBe` fromIntegral <$> expected
--
---- Test cases adapted from `exercism/x-common/wordy.json` on 2016-08-10.
--
--data Case = Case { description :: String
--                 , input       :: String
--                 , expected    :: Maybe Integer
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "addition"
--               , input       = "What is 1 plus 1?"
--               , expected    = Just 2
--               }
--        , Case { description = "more addition"
--               , input       = "What is 53 plus 2?"
--               , expected    = Just 55
--               }
--        , Case { description = "addition with negative numbers"
--               , input       = "What is -1 plus -10?"
--               , expected    = Just (-11)
--               }
--        , Case { description = "large addition"
--               , input       = "What is 123 plus 45678?"
--               , expected    = Just 45801
--               }
--        , Case { description = "subtraction"
--               , input       = "What is 4 minus -12?"
--               , expected    = Just 16
--               }
--        , Case { description = "multiplication"
--               , input       = "What is -3 multiplied by 25?"
--               , expected    = Just (-75)
--               }
--        , Case { description = "division"
--               , input       = "What is 33 divided by -3?"
--               , expected    = Just (-11)
--               }
--        , Case { description = "multiple additions"
--               , input       = "What is 1 plus 1 plus 1?"
--               , expected    = Just 3
--               }
--        , Case { description = "addition and subtraction"
--               , input       = "What is 1 plus 5 minus -2?"
--               , expected    = Just 8
--               }
--        , Case { description = "multiple subtraction"
--               , input       = "What is 20 minus 4 minus 13?"
--               , expected    = Just 3
--               }
--        , Case { description = "subtraction then addition"
--               , input       = "What is 17 minus 6 plus 3?"
--               , expected    = Just 14
--               }
--        , Case { description = "multiple multiplication"
--               , input       = "What is 2 multiplied by -2 multiplied by 3?"
--               , expected    = Just (-12)
--               }
--        , Case { description = "addition and multiplication"
--               , input       = "What is -3 plus 7 multiplied by -2?"
--               , expected    = Just (-8)
--               }
--        , Case { description = "multiple division"
--               , input       = "What is -12 divided by 2 divided by -3?"
--               , expected    = Just 2
--               }
--        , Case { description = "unknown operation"
--               , input       = "What is 52 cubed?"
--               , expected    = Nothing
--               }
--        , Case { description = "Non math question"
--               , input       = "Who is the President of the United States?"
--               , expected    = Nothing
--               }
--        ]

---- Nucleotide counts
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "nucleotide-count" $ do
--
--    -- As of 2016-07-27, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    let x `matches`    y = x `shouldBe`  Right y
--    let x `matchesMap` y = x `shouldBe` (Right . fromList) y
--
--    describe "count" $ do
--
--      it "empty dna strand has no adenosine" $
--        count 'A' "" `matches` 0
--
--      it "repetitive cytidine gets counted" $
--        count 'C' "CCCCC" `matches` 5
--
--      it "counts only thymidine" $
--        count 'T' "GGGGGTAACCCGG" `matches` 1
--
--      it "validates nucleotides" $
--        count 'X' "GACT" `shouldSatisfy` isLeft
--
--      it "validates strand" $
--        count 'G' "GACYT" `shouldSatisfy` isLeft
--
--    describe "nucleotideCounts" $ do
--
--      it "empty dna strand has no nucleotides" $
--        nucleotideCounts "" `matchesMap` [ ('A', 0)
--                                         , ('C', 0)
--                                         , ('G', 0)
--                                         , ('T', 0) ]
--
--      it "repetitive-sequence-has-only-guanosine" $
--        nucleotideCounts "GGGGGGGG" `matchesMap` [ ('A', 0)
--                                                 , ('C', 0)
--                                                 , ('G', 8)
--                                                 , ('T', 0) ]
--
--      it "counts all nucleotides" $
--        nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
--        `matchesMap` [ ('A', 20)
--                     , ('C', 12)
--                     , ('G', 17)
--                     , ('T', 21) ]
--
--      it "validates strand" $
--        nucleotideCounts "GPAC" `shouldSatisfy` isLeft
--
---- Secret Handshake
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "secret-handshake" $ do
--
--    -- As of 2016-09-12, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    it "1 to wink" $ do
--      handshake (1 :: Int) `shouldBe` ["wink"]
--      handshake "1"        `shouldBe` ["wink"]
--
--    it "10 to double blink" $ do
--      handshake (2 :: Int) `shouldBe` ["double blink"]
--      handshake "10"       `shouldBe` ["double blink"]
--
--    it "100 to close your eyes" $ do
--      handshake (4 :: Int) `shouldBe` ["close your eyes"]
--      handshake "100"      `shouldBe` ["close your eyes"]
--
--    it "1000 to jump" $ do
--      handshake (8 :: Int) `shouldBe` ["jump"]
--      handshake "1000"     `shouldBe` ["jump"]
--
--    it "11 to wink and double blink" $ do
--      handshake (3 :: Int) `shouldBe` ["wink", "double blink"]
--      handshake "11"       `shouldBe` ["wink", "double blink"]
--
--    it "10011 to double blink and wink" $ do
--      handshake (19 :: Int) `shouldBe` ["double blink", "wink"]
--      handshake "10011"     `shouldBe` ["double blink", "wink"]
--
--    it "11111 to jump, close your eyes, double blink, and wink" $ do
--      handshake (31 :: Int) `shouldBe` ["jump", "close your eyes", "double blink", "wink"]
--      handshake "11111"     `shouldBe` ["jump", "close your eyes", "double blink", "wink"]
--
--    it "zero" $ do
--      handshake (0 :: Int) `shouldBe` []
--      handshake "0"        `shouldBe` []
--
--    it "gibberish" $
--      handshake "piggies" `shouldBe` []
--
--    it "partial gibberish" $
--      handshake "1piggies" `shouldBe` []
-- RNA
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "rna-transcription" $
--          describe "toRNA" $ for_ cases test
--  where
--    test Case{..} = it description $ toRNA dna `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common/rna-transcription.json` on 2016-07-24.
--
--data Case = Case { description ::       String
--                 , dna         ::       String
--                 , expected    :: Maybe String
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "rna complement of cytosine is guanine"
--               , dna         =      "C"
--               , expected    = Just "G"
--               }
--        , Case { description = "rna complement of guanine is cytosine"
--               , dna         =      "G"
--               , expected    = Just "C"
--               }
--        , Case { description = "rna complement of thymine is adenine"
--               , dna         =      "T"
--               , expected    = Just "A"
--               }
--        , Case { description = "rna complement of adenine is uracil"
--               , dna         =      "A"
--               , expected    = Just "U"
--               }
--        , Case { description = "rna complement"
--               , dna         =      "ACGTGGTCTTAA"
--               , expected    = Just "UGCACCAGAAUU"
--               }
--        , Case { description = "dna correctly handles invalid input"
--               , dna         = "U"
--               , expected    = Nothing
--               }
--        , Case { description = "dna correctly handles completely invalid input"
--               , dna         = "XXX"
--               , expected    = Nothing
--               }
--        , Case { description = "dna correctly handles partially invalid input"
--               , dna         = "ACGTXXXCTTAA"
--               , expected    = Nothing
--               }
--        ]
--

---- Sublist
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "sublist" $ do
--          describe "standard tests" $ for_ cases test
--          describe "track specific tests" $ do
--
--            let xs = replicate 1000 'x'
--
--            it "compare larger equal lists" $
--              sublist xs xs
--              `shouldBe` Equal
--
--            it "sublist early in huge list" $
--              sublist [3, 4, 5] [1..1000000 :: Int]
--              `shouldBe` Sublist
--
--            it "huge sublist not in huge list" $
--              sublist [10..1000001] [1..1000000 :: Int]
--              `shouldBe` Unequal
--
--            it "superlist early in huge list" $
--              sublist [1..1000000] [3, 4, 5 :: Int]
--              `shouldBe` Superlist
--  where
--
--    test Case{..} = it explanation assertion
--      where
--        assertion   = sublist listOne listTwo `shouldBe` expectation
--        explanation = unwords [ "sublist"
--                              , show listOne
--                              , show listTwo
--                              , "-"
--                              , description ]
--
---- Test cases adapted from `exercism/x-common/sublist.json` on 2016-07-27.
--
--data Case = Case { description :: String
--                 , listOne     :: [Integer]
--                 , listTwo     :: [Integer]
--                 , expectation :: Sublist
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "empty lists"
--               , listOne     = []
--               , listTwo     = []
--               , expectation = Equal
--               }
--        , Case { description = "empty list within non empty list"
--               , listOne     = []
--               , listTwo     = [1, 2, 3]
--               , expectation = Sublist
--               }
--        , Case { description = "non empty list contains empty list"
--               , listOne     = [1, 2, 3]
--               , listTwo     = []
--               , expectation = Superlist
--               }
--        , Case { description = "list equals itself"
--               , listOne     = [1, 2, 3]
--               , listTwo     = [1, 2, 3]
--               , expectation = Equal
--               }
--        , Case { description = "different lists"
--               , listOne     = [1, 2, 3]
--               , listTwo     = [2, 3, 4]
--               , expectation = Unequal
--               }
--        , Case { description = "false start"
--               , listOne     = [1, 2, 5]
--               , listTwo     = [0, 1, 2, 3, 1, 2, 5, 6]
--               , expectation = Sublist
--               }
--        , Case { description = "consecutive"
--               , listOne     = [1, 1, 2]
--               , listTwo     = [0, 1, 1, 1, 2, 1, 2]
--               , expectation = Sublist
--               }
--        , Case { description = "sublist at start"
--               , listOne     = [0, 1, 2]
--               , listTwo     = [0, 1, 2, 3, 4, 5]
--               , expectation = Sublist
--               }
--        , Case { description = "sublist in middle"
--               , listOne     = [2, 3, 4]
--               , listTwo     = [0, 1, 2, 3, 4, 5]
--               , expectation = Sublist
--               }
--        , Case { description = "sublist at end"
--               , listOne     = [3, 4, 5]
--               , listTwo     = [0, 1, 2, 3, 4, 5]
--               , expectation = Sublist
--               }
--        , Case { description = "at start of superlist"
--               , listOne     = [0, 1, 2, 3, 4, 5]
--               , listTwo     = [0, 1, 2]
--               , expectation = Superlist
--               }
--        , Case { description = "in middle of superlist"
--               , listOne     = [0, 1, 2, 3, 4, 5]
--               , listTwo     = [2, 3]
--               , expectation = Superlist
--               }
--        , Case { description = "at end of superlist"
--               , listOne     = [0, 1, 2, 3, 4, 5]
--               , listTwo     = [3, 4, 5]
--               , expectation = Superlist
--               }
--        , Case { description = "first list missing element from second list"
--               , listOne     = [1, 3]
--               , listTwo     = [1, 2, 3]
--               , expectation = Unequal
--               }
--        , Case { description = "second list missing element from first list"
--               , listOne     = [1, 2, 3]
--               , listTwo     = [1, 3]
--               , expectation = Unequal
--               }
--        , Case { description = "order matters to a list"
--               , listOne     = [1, 2, 3]
--               , listTwo     = [3, 2, 1]
--               , expectation = Unequal
--               }
--        ]
--
---- GRains
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "grains" $ do
--          describe "square" $ for_ squareCases squareTest
--          describe "total"  $ totalTest totalCase
--  where
--
--    squareTest (description, n, expected) = it description assertion
--      where
--        assertion  = expression `shouldBe` expected
--        expression = fmap fromIntegral . square . fromIntegral $ n
--
--    totalTest (description, expected) = it description assertion
--      where
--        assertion = fromIntegral total `shouldBe` expected
--
---- As of 2016-07-27, there was no reference file
---- for the test cases in `exercism/x-common`.
--
--squareCases :: [(String, Integer, Maybe Integer)]
--squareCases =
--    [ ("square 1"             ,  1, Just                   1)
--    , ("square 2"             ,  2, Just                   2)
--    , ("square 3"             ,  3, Just                   4)
--    , ("square 4"             ,  4, Just                   8)
--    , ("square 16"            , 16, Just               32768)
--    , ("square 32"            , 32, Just          2147483648)
--    , ("square 64"            , 64, Just 9223372036854775808)
--    , ("square negative"      , -1, Nothing                 )
--    , ("square 0"             ,  0, Nothing                 )
--    , ("square bigger than 64", 65, Nothing                 ) ]
--
--totalCase :: (String, Integer)
--totalCase = ("total grains", 18446744073709551615)
--
-- AtBash
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "atbash-cipher" $ do
--          describe "encode" $ for_ encodeCases $ test encode
--          describe "decode" $ for_ decodeCases $ test decode
--  where
--    test f Case{..} = it description $ f phrase `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common/atbash-cipher.json` on 2016-08-02.
--
--data Case = Case { description :: String
--                 , phrase      :: String
--                 , expected    :: String
--                 }
--
--encodeCases :: [Case]
--encodeCases =
--    [ Case { description = "encode yes"
--           , phrase      = "yes"
--           , expected    = "bvh"
--           }
--    , Case { description = "encode no"
--           , phrase      = "no"
--           , expected    = "ml"
--           }
--    , Case { description = "encode OMG"
--           , phrase      = "OMG"
--           , expected    = "lnt"
--           }
--    , Case { description = "encode spaces"
--           , phrase      = "O M G"
--           , expected    = "lnt"
--           }
--    , Case { description = "encode mindblowingly"
--           , phrase      = "mindblowingly"
--           , expected    = "nrmwy oldrm tob"
--           }
--    , Case { description = "encode numbers"
--           , phrase      = "Testing,1 2 3, testing."
--           , expected    = "gvhgr mt123 gvhgr mt"
--           }
--    , Case { description = "encode deep thought"
--           , phrase      = "Truth is fiction."
--           , expected    = "gifgs rhurx grlm"
--           }
--    , Case { description = "encode all the letters"
--           , phrase      = "The quick brown fox jumps over the lazy dog."
--           , expected    = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
--           }
--    , Case { description = "encode ignores non ascii"
--           , phrase      = "non ascii éignored"
--           , expected    = "mlmzh xrrrt mlivw"
--           }
--    ]
--
--decodeCases :: [Case]
--decodeCases =
--    [ Case { description = "decode exercism"
--           , phrase      = "vcvix rhn"
--           , expected    = "exercism"
--           }
--    , Case { description = "decode a sentence"
--           , phrase      = "zmlyh gzxov rhlug vmzhg vkkrm thglm v"
--           , expected    = "anobstacleisoftenasteppingstone"
--           }
--    , Case { description = "decode numbers"
--           , phrase      = "gvhgr mt123 gvhgr mt"
--           , expected    = "testing123testing"
--           }
--    , Case { description = "decode all the letters"
--           , phrase      = "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"
--           , expected    = "thequickbrownfoxjumpsoverthelazydog"
--           }
--    ]
--
-- Accumulate
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "accumulate" $ do
--
--    -- As of 2016-07-27, there was no reference file
--    -- for the test cases in `exercism/x-common`.
--
--    let square x = x * x :: Int
--
--    it "empty accumulation" $
--      accumulate square []
--      `shouldBe` []
--
--    it "accumulate squares" $
--      accumulate square [1, 2, 3]
--      `shouldBe` [1, 4, 9]
--
--    it "accumulate upcases" $
--      accumulate (map toUpper) ["hello", "world"]
--      `shouldBe` ["HELLO", "WORLD"]
--
--    it "accumulate reversed strings" $
--      accumulate reverse ["the", "quick", "brown", "fox", "etc"]
--      `shouldBe` ["eht", "kciuq", "nworb", "xof", "cte"]
--
--    it "accumulate recursively" $
--      accumulate (\c -> accumulate ((c:) . show) ([1, 2, 3] :: [Int])) "abc"
--      `shouldBe` [["a1", "a2", "a3"], ["b1", "b2", "b3"], ["c1", "c2", "c3"]]
--
--    it "accumulate non-strict" $
--      take 1 (accumulate id ("nice work!" : error "accumulate should be even lazier, don't use reverse!"))
--      `shouldBe` ["nice work!"]
--
--exitProperly :: IO Counts -> IO ()
--exitProperly m = do
--  counts <- m
--  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess
--
--testCase :: String -> Assertion -> Test
--testCase label assertion = TestLabel label (TestCase assertion)

-- SpaceAge
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "space-age" $
--          describe "ageOn" $ for_ cases test
--  where
--
--    test Case{..} = it description assertion
--      where
--        expression  = ageOn planet $ fromIntegral seconds
--        assertion   = roundAge expression `shouldBe` roundAge expected
--        roundAge    = (/ 100) . fromIntegral . round . (* 100)
--
---- Test cases adapted from `exercism/x-common/space-age.json` on 2016-07-27.
--
--data Case = Case { description :: String
--                 , planet      :: Planet
--                 , seconds     :: Integer
--                 , expected    :: Double
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "Earth"
--               , planet      = Earth
--               , seconds     = 1000000000
--               , expected    = 31.69
--               }
--        , Case { description = "Mercury"
--               , planet      = Mercury
--               , seconds     = 2134835688
--               , expected    = 280.88
--               }
--        , Case { description = "Venus"
--               , planet      = Venus
--               , seconds     = 189839836
--               , expected    = 9.78
--               }
--        , Case { description = "Mars"
--               , planet      = Mars
--               , seconds     = 2329871239
--               , expected    = 39.25
--               }
--        , Case { description = "Jupiter"
--               , planet      = Jupiter
--               , seconds     = 901876382
--               , expected    = 2.41
--               }
--        , Case { description = "Saturn"
--               , planet      = Saturn
--               , seconds     = 3000000000
--               , expected    = 3.23
--               }
--        , Case { description = "Uranus"
--               , planet      = Uranus
--               , seconds     = 3210123456
--               , expected    = 1.21
--               }
--        , Case { description = "Neptune"
--               , planet      = Neptune
--               , seconds     = 8210123456
--               , expected    = 1.58
--               }
--        ]
--
-- Bob
--main :: IO ()
--main = hspecWith defaultConfig {configFastFail = True} specs
--
--specs :: Spec
--specs = describe "bob" $
--          describe "responseFor" $ for_ cases test
--  where
--    test Case{..} = it description $ responseFor input `shouldBe` expected
--
---- Test cases adapted from `exercism/x-common/bob.json` on 2016-07-24.
--
--data Case = Case { description :: String
--                 , input       :: String
--                 , expected    :: String
--                 }
--
--cases :: [Case]
--cases = [ Case { description = "stating something"
--               , input       = "Tom-ay-to, tom-aaaah-to."
--               , expected    = "Whatever."
--               }
--        , Case { description = "shouting"
--               , input       = "WATCH OUT!"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "shouting gibberish"
--               , input       = "FCECDFCAAB"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "asking a question"
--               , input       = "Does this cryogenic chamber make me look fat?"
--               , expected    = "Sure."
--               }
--        , Case { description = "asking a numeric question"
--               , input       = "You are, what, like 15?"
--               , expected    = "Sure."
--               }
--        , Case { description = "asking gibberish"
--               , input       = "fffbbcbeab?"
--               , expected    = "Sure."
--               }
--        , Case { description = "talking forcefully"
--               , input       = "Let's go make out behind the gym!"
--               , expected    = "Whatever."
--               }
--        , Case { description = "using acronyms in regular speech"
--               , input       = "It's OK if you don't want to go to the DMV."
--               , expected    = "Whatever."
--               }
--        , Case { description = "forceful question"
--               , input       = "WHAT THE HELL WERE YOU THINKING?"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "shouting numbers"
--               , input       = "1, 2, 3 GO!"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "only numbers"
--               , input       = "1, 2, 3"
--               , expected    = "Whatever."
--               }
--        , Case { description = "question with only numbers"
--               , input       = "4?"
--               , expected    = "Sure."
--               }
--        , Case { description = "shouting with special characters"
--               , input       = "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "shouting with umlauts"
--               , input       = "ÜMLÄÜTS!"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "calmly speaking with umlauts"
--               , input       = "ÜMLäÜTS!"
--               , expected    = "Whatever."
--               }
--        , Case { description = "shouting with no exclamation mark"
--               , input       = "I HATE YOU"
--               , expected    = "Whoa, chill out!"
--               }
--        , Case { description = "statement containing question mark"
--               , input       = "Ending with ? means a question."
--               , expected    = "Whatever."
--               }
--        , Case { description = "non-letters with question"
--               , input       = ":) ?"
--               , expected    = "Sure."
--               }
--        , Case { description = "prattling on"
--               , input       = "Wait! Hang on. Are you going to be OK?"
--               , expected    = "Sure."
--               }
--        , Case { description = "silence"
--               , input       = ""
--               , expected    = "Fine. Be that way!"
--               }
--        , Case { description = "prolonged silence"
--               , input       = "          "
--               , expected    = "Fine. Be that way!"
--               }
--        , Case { description = "alternate silence"
--               , input       = "\t\t\t\t\t\t\t\t\t\t"
--               , expected    = "Fine. Be that way!"
--               }
--        , Case { description = "multiple line question"
--               , input       = "\nDoes this cryogenic chamber make me look fat?\nno"
--               , expected    = "Whatever."
--               }
--        , Case { description = "starting with whitespace"
--               , input       = "         hmmmmmmm..."
--               , expected    = "Whatever."
--               }
--        , Case { description = "ending with whitespace"
--               , input       = "Okay if like my  spacebar  quite a bit?   "
--               , expected    = "Sure."
--               }
--        , Case { description = "other whitespace"
--               , input       = "\n\r \t\x000b\x00a0\x2002"
--               , expected    = "Fine. Be that way!"
--               }
--        , Case { description = "non-question ending with whitespace"
--               , input       = "This is a statement ending with whitespace      "
--               , expected    = "Whatever."
--               }
--        ]
-- SumOfMultiples
--main :: IO ()
--main = exitProperly $ runTestTT $ TestList
--       [ TestList sumOfMultiplesTests ]
--
---- Note that the upper bound is not included in the result
--sumOfMultiplesTests :: [Test]
--sumOfMultiplesTests =
--  [ testCase "1" $
--    0 @=? sumOfMultiples [3, 5] 1
--  , testCase "4" $
--    3 @=? sumOfMultiples [3, 5] 4
--  , testCase "10" $
--    23 @=? sumOfMultiples [3, 5] 10
--  , testCase "1000" $
--    2318 @=? sumOfMultiples [3, 5] 100
--  , testCase "1000" $
--    233168 @=? sumOfMultiples [3, 5] 1000
--  , testCase "[7, 13, 17] 20" $
--    51 @=? sumOfMultiples [7, 13, 17] 20
--  , testCase "[4, 6] 15" $
--    30 @=? sumOfMultiples [4, 6] 15
--  , testCase "[5, 6, 8] 150" $
--    4419 @=? sumOfMultiples [5, 6, 8] 150
--  , testCase "[43, 47] 10000" $
--    275 @=? sumOfMultiples [5,25] 51
--  , testCase "[1] 100" $
--    2203160 @=? sumOfMultiples [43, 47] 10000
--  , testCase "[5, 25] 51" $
--    4950 @=? sumOfMultiples [1] 100
--  , testCase "[] 10000" $
--    0 @=? sumOfMultiples [] 10000
--  ]

-- Word square
--main :: IO ()
--main = exitProperly $ runTestTT $ TestList
--       [ TestLabel "normalizePlaintext" $ TestList normalizePlaintextTests
--       , TestLabel "squareSize" $ TestList squareSizeTests
--       , TestLabel "plaintextSegments" $ TestList plaintextSegmentsTests
--       , TestLabel "ciphertext" $ TestList ciphertextTests
--       , TestLabel "normalizeCiphertext" $ TestList normalizeCiphertextTests
--       ]
--
--normalizePlaintextTests :: [Test]
--normalizePlaintextTests = map TestCase
--  [ "splunk" @=? normalizePlaintext "s#!@$%plunk"
--  , "123go" @=? normalizePlaintext "1, 2, 3 GO!"
--  ]
--
--squareSizeTests :: [Test]
--squareSizeTests = map TestCase
--  [ 2 @=? squareSize "1234"
--  , 3 @=? squareSize "123456789"
--  , 4 @=? squareSize "123456789abc" ]
--
--plaintextSegmentsTests :: [Test]
--plaintextSegmentsTests = map TestCase
--  [ ["neverv", "exthin", "eheart", "withid", "lewoes"] @=?
--    plaintextSegments "Never vex thine heart with idle woes."
--  , ["zomg", "zomb", "ies"] @=?
--    plaintextSegments "ZOMG! ZOMBIES!!!"
--  ]
--
--ciphertextTests :: [Test]
--ciphertextTests = map TestCase
--  [ "tasneyinicdsmiohooelntuillibsuuml" @=?
--    ciphertext "Time is an illusion. Lunchtime doubly so."
--  , "wneiaweoreneawssciliprerlneoidktcms" @=?
--    ciphertext "We all know interspecies romance is weird."
--  ]
--
--normalizeCiphertextTests :: [Test]
--normalizeCiphertextTests = map TestCase
--  [ "msemo aanin dnin ndla etlt shui" @=?
--    normalizeCiphertext "Madness, and then illumination."
--  , "vrel aepe mset paoo irpo" @=?
--     normalizeCiphertext "Vampires are people too!"
--  , "imtgdvs fearwer mayoogo anouuio ntnnlvt wttddes aohghn sseoau" @=?
--     normalizeCiphertext "If man was meant to stay on the ground god would \
--                         \have given us roots"
--  ]

-- Wordcount
-- main :: IO ()
-- main = exitProperly (runTestTT (TestList wordCountTests))
-- 
-- wordCountTests :: [Test]
-- wordCountTests =
--   [ testCase "count one word" $
--     fromList [("word", 1)] @=? wordCount "word"
--   , testCase "count one of each" $
--     fromList [("one", 1), ("of", 1), ("each", 1)] @=? wordCount "one of each"
--   , testCase "count multiple occurrences" $
--     fromList [("one", 1), ("fish", 4), ("two", 1),
--               ("red", 1), ("blue", 1)] @=?
--     wordCount "one fish two fish red fish blue fish"
--   , testCase "ignore punctuation" $
--     fromList [("car", 1), ("carpet", 1), ("as", 1),
--               ("java", 1), ("javascript", 1)] @=?
--     wordCount "car : carpet as java : javascript!!&@$%^&"
--   , testCase "include numbers" $
--     fromList [("testing", 2), ("1", 1), ("2", 1)] @=?
--     wordCount "testing, 1, 2 testing"
--   , testCase "normalize case" $
--     fromList [("go", 3)] @=? wordCount "go Go GO"
--   , testCase "prefix punctuation" $
--     fromList [("testing", 2), ("1", 1), ("2", 1)] @=?
--     wordCount "!%%#testing, 1, 2 testing"
--   , testCase "symbols are separators" $
--     fromList [("hey", 1), ("my", 1), ("spacebar", 1),
--               ("is", 1), ("broken", 1)] @=?
--     wordCount "hey,my_spacebar_is_broken."
--   ]

-- Anagram
--main :: IO ()
--main = exitProperly (runTestTT (TestList anagramTests))
--
--anagramTests :: [Test]
--anagramTests =
--  [ testCase "no matches" $
--    [] @=? anagramsFor "diaper" ["hello", "world", "zombies", "pants"]
--  , testCase "detect simple anagram" $
--    ["tan"] @=? anagramsFor "ant" ["tan", "stand", "at"]
--  , testCase "does not confuse different duplicates" $
--    [] @=? anagramsFor "galea" ["eagle"]
--  , testCase "eliminate anagram subsets" $
--    [] @=? anagramsFor "good" ["dog", "goody"]
--  , testCase "detect anagram" $
--    ["inlets"] @=? anagramsFor "listen" ["enlists", "google",
--                                         "inlets", "banana"]
--  , testCase "multiple anagrams" $
--    ["gallery", "regally", "largely"] @=?
--    anagramsFor "allergy" ["gallery", "ballerina", "regally", "clergy",
--                           "largely", "leading"]
--  , testCase "case insensitive anagrams" $
--    ["Carthorse"] @=?
--    anagramsFor "Orchestra" ["cashregister", "Carthorse", "radishes"]
--  , testCase "does not detect a word as its own anagram" $
--    [] @=? anagramsFor "banana" ["banana"]
--  , testCase "does not detect a word as its own anagram (case insensitive)" $
--    [] @=? anagramsFor "Banana" ["baNana"]
--  ]
--

-- Beer
-- main :: IO ()
-- main = exitProperly (runTestTT (TestList [TestList verseTests, TestList singTests]))
-- 
-- verse_8, verse_2, verse_1, verse_0 :: String
-- verse_8 = "8 bottles of beer on the wall, 8 bottles of beer.\nTake one down and pass it around, 7 bottles of beer on the wall.\n"
-- verse_2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
-- verse_1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
-- verse_0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
-- 
-- song_8_6, song_3_0 :: String
-- song_8_6 = "8 bottles of beer on the wall, 8 bottles of beer.\nTake one down and pass it around, 7 bottles of beer on the wall.\n\n7 bottles of beer on the wall, 7 bottles of beer.\nTake one down and pass it around, 6 bottles of beer on the wall.\n\n6 bottles of beer on the wall, 6 bottles of beer.\nTake one down and pass it around, 5 bottles of beer on the wall.\n\n"
-- song_3_0 = "3 bottles of beer on the wall, 3 bottles of beer.\nTake one down and pass it around, 2 bottles of beer on the wall.\n\n2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n\n1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n\nNo more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n\n"
-- 
-- 
-- verseTests :: [Test]
-- verseTests =
--   [ testCase "verse 8" $ verse_8 @=? verse 8
--   , testCase "verse 2" $ verse_2 @=? verse 2
--   , testCase "verse 1" $ verse_1 @=? verse 1
--   , testCase "verse 0" $ verse_0 @=? verse 0
--   ]
-- 
-- singTests :: [Test]
-- singTests =
--   [ testCase "song 8 6" $ song_8_6 @=? sing 8 6
--   , testCase "song 3 0" $ song_3_0 @=? sing 3 0
--   ]
