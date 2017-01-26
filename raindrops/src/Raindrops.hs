module Raindrops (convert) where

convert s
    | (s `mod` 3 > 0) && (s `mod` 5 > 0) && (s `mod` 7 > 0) = show s
    | otherwise = add 3 "Pling" ++ add 5 "Plang" ++ add 7 "Plong"
    where
      add x m | s `mod` x == 0 = m | otherwise = ""
