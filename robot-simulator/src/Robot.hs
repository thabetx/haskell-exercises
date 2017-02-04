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
advance b (x, y)
    | b == North = (x, y+1)
    | b == East  = (x+1, y)
    | b == South = (x, y-1)
    | b == West  = (x-1, y)

update :: Robot -> Char -> Robot
update (Robot b c) command
    | command == 'A' = mkRobot b $ advance b c
    | command == 'L' = mkRobot (turnLeft b) c
    | command == 'R' = mkRobot (turnRight b) c

simulate :: Robot -> String -> Robot
simulate robot commands = foldl update robot commands

turnLeft :: Bearing -> Bearing
turnLeft b
    | b == North = West
    | b == West  = South
    | b == South = East
    | b == East  = North

turnRight :: Bearing -> Bearing
turnRight b
    | b == North = East
    | b == East  = South
    | b == South = West
    | b == West  = North
