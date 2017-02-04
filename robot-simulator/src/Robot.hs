module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { robotBearing :: Bearing
                   , robotCoordinates :: (Integer, Integer)
                   } deriving (Show)

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance b (x, y) = case b of
    North -> (x, y+1)
    East  -> (x+1, y)
    South -> (x, y-1)
    West  -> (x-1, y)

update :: Robot -> Char -> Robot
update (Robot b c) command = case command of
    'A' ->  mkRobot b $ advance b c
    'L' ->  mkRobot (turnLeft b) c
    'R' ->  mkRobot (turnRight b) c

simulate :: Robot -> String -> Robot
simulate robot commands = foldl update robot commands

turnLeft :: Bearing -> Bearing
turnLeft b = case b of
    North -> West
    West  -> South
    South -> East
    East  -> North

turnRight :: Bearing -> Bearing
turnRight b = case b of
    North -> East
    East  -> South
    South -> West
    West  -> North
