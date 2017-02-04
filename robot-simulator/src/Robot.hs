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

advance :: Bearing -> (Integer, Integer)
advance b
    | b == North = (0, 1)
    | b == East  = (1, 0)
    | b == South = (0, -1)
    | b == West  = (-1, 0)

add :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
add a b = (fst a + fst b, snd a + snd b)

update :: Robot -> Char -> Robot
update robot command
    | command == 'A' = mkRobot b $ add c $ advance b
    | command == 'L' = mkRobot (turnLeft b) c
    | command == 'R' = mkRobot (turnRight b)c
    where b = robotBearing robot
          c = robotCoordinates robot

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
