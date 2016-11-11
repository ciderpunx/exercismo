module Meetup (Schedule(..), Weekday(..), meetupDay) where
import Data.Time

data Schedule = First | Second | Third | Fourth | Fifth | Last | Teenth
data Weekday  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay nth dow year month =
    case nth of
      Teenth -> head possibleDows
      First  -> head possibleDows
      Second -> possibleDows !! 1
      Third  -> possibleDows !! 2
      Fourth -> possibleDows !! 3
      Fifth  -> possibleDows !! 4
      Last   -> last possibleDows
  where
    day'            = toDayString dow
    dayRange        = map (makeDate year month) (getRange nth)
    getRange Teenth = [13..19]
    getRange nth    = [1..31]
    possibleDows    = map utctDay $
                        filter ((==day')
                        . formatTime defaultTimeLocale "%A") dayRange

toDayString :: Weekday -> String
toDayString Monday    = "Monday"
toDayString Tuesday   = "Tuesday"
toDayString Wednesday = "Wednesday"
toDayString Thursday  = "Thursday"
toDayString Friday    = "Friday"
toDayString Saturday  = "Saturday"
toDayString Sunday    = "Sunday"

-- note that fromGregorian will return the last day in a month if you give it a 
-- number higher than the number of days in the month. This means we can throw
-- 2015 2 31 at it and get back 28 Feb 2015
makeDate :: Integer -> Int -> Int -> UTCTime
makeDate y m d = UTCTime (fromGregorian y m d) (fromIntegral $ 12 * 3600)
