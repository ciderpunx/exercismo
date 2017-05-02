module Bowling where
import Control.Applicative ((<$>))

-- I'd like to have an InvalidGame error here too, for times when we have too many rolls
-- or frames or that sort of problem...
data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
                  deriving (Eq, Show)

-- Given a list of rolls, return a score or BowlingError, error out on obviously invalid input
score :: [Int] -> Either BowlingError Int
score rs
    | length rs > 30     = Left IncompleteGame -- more than 3 rolls per frame is impossible
    | any invalidRoll rs = Left InvalidRoll { rollIndex = length validRolls, rollValue = firstInvalidRoll }
    | otherwise          = score' 0 10 rs
  where
    invalidRoll r     = r < 0 || r > 10
    validRolls        = takeWhile (not . invalidRoll) rs
    firstInvalidRoll  = head $ filter invalidRoll rs
    numStrikes        = length $ filter (==10) rs

-- Given a start index a number of frames and a list of rolls,
-- return a score or a Bowling error
score' :: Int -> Int -> [Int] -> Either BowlingError Int
score' i f (a:b:c:xs)
      -- Strike in normal practice
    | a == 10
      && f > 1    = completeGameScore (a + b + c + ) (i+3) f (b:c:xs)
      -- Strike, last frame -- bonus rolls shouldn't add up to more than 10
      -- unless first is a strike
    | a == 10
      && f == 1
      && b /= 10
      && b+c > 10 = Left InvalidRoll {rollIndex = i+2, rollValue = c}
      -- Strike, last frame
    | a == 10
      && f == 1   = completeGameScore (a + b + c + ) (i+3) f []
      -- Shouldn't be able to knock down more than 10 pins in 2 rolls,
      -- unless first was strike (and hence already dealt with)
    | a + b > 10  = Left InvalidRoll {rollIndex = i+1, rollValue = b}
      -- Spare
    | a + b == 10 = completeGameScore (10 + c + ) (i+3) f (if f > 1 then c:xs else [])
      -- Open frame
    | otherwise   = completeGameScore (a + b +) (i+2) f (c:xs)

-- Last 2 rolls -- can be bonuses, should only occur on last frame
score' i f (x:y:[])
      -- Last frame, not a strike or a spare
    | f == 1
      && x /= 10
      && x + y /= 10 = Right (x + y)
      -- Not last frame or last frame and we got a strike or spare
      -- but did not take bonus roll
    | otherwise      = Left IncompleteGame

-- last roll, oughtn't to be a strike, which should finish on the end of game equation
score' i f (x:[])
      -- Went past end of game
    | f < 0           = Left IncompleteGame
      -- Last frame, strike but no bonus roll taken
    | f == 1 && x==10 = Left IncompleteGame
      -- Last roll in last frame
    | f == 1          = Right x
      -- Can only be invalid
    | otherwise       = Left InvalidRoll {rollIndex = i, rollValue = x}

---- End of game -- our frame ought to be 0 (i.e. we've played 10 frames in total)
score' i f []
      -- Frame is 0 because we play 10 total
    | f == 0 = Right 0
      -- More or less frames than expected
    | otherwise = Left IncompleteGame

-- Increment the score and recur (fmap over Either)
completeGameScore fn i frames xs =
    fn <$> score' i (frames-1) xs
