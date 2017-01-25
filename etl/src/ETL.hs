module ETL (transform) where

import Data.Map (Map, toList, fromList)
import Data.Char

flatten :: (a, String) -> [(Char, a)]
flatten x = foldl (\acc y -> acc ++ [(toLower y, fst x)]) [] $ snd x

transform :: Map a String -> Map Char a
transform old = fromList $ foldl (\acc x -> acc ++ (flatten x) ) [] $ toList old
