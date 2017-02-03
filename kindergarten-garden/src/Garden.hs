module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, insert, empty, lookup)
import Data.Maybe (fromJust)
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

plantName :: Char -> Plant
plantName 'C' = Clover
plantName 'G' = Grass
plantName 'R' = Radishes
plantName 'V' = Violets

convert :: String -> [Plant]
convert [] = [];
convert (x:xs) = plantName x : convert xs

dissect :: String -> Int -> String
dissect g index = foldl (\ acc x -> acc ++ (take 2 $ drop (2*index) x)) "" $ lines g

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden defaultStudents

garden :: [String] -> String -> Map String [Plant]
garden students g = foldl (\ acc index -> insert (sorted !! index) (convert $ dissect g index) acc)
                    Data.Map.empty [0.. length students - 1]
                    where sorted = sort students

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants name g = fromJust $ Data.Map.lookup name g
