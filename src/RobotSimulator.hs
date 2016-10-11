module RobotSimulator where

data Bearing = North | East | South | West deriving (Show, Eq, Enum)

type Location = (Int, Int)

data Robot = Robot Bearing Location deriving Show

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight b    = succ b

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft b     = pred b

left :: Robot -> Robot
left (Robot b l) = Robot (turnLeft b) l

right :: Robot -> Robot
right (Robot b l) = Robot (turnRight b) l

advance :: Robot -> Robot
advance (Robot North (x,y)) = Robot North (x,   y+1)
advance (Robot East  (x,y)) = Robot East  (x+1, y)
advance (Robot South (x,y)) = Robot South (x,   y-1)
advance (Robot West  (x,y)) = Robot West  (x-1, y)

charToMove :: Char -> Robot -> Robot
charToMove 'A' = advance
charToMove 'R' = right
charToMove 'L' = left

moveBot :: Robot -> String -> Robot
moveBot = foldl (flip charToMove)

-- Testing functions from exercism.io
bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> Location
coordinates (Robot _ (x,y)) = (x,y)

simulate :: Robot -> String -> Robot
simulate = moveBot

mkRobot :: Bearing -> Location -> Robot
mkRobot = Robot
