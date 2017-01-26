module Raindrops (convert) where

convert s
    | (mod s 3 > 0) && (mod s 5 > 0) && (mod s 7 > 0) = show s
    | otherwise = x ++ y ++ z
    where
      x | mod s 3 == 0 = "Pling" | otherwise = ""
      y | mod s 5 == 0 = "Plang" | otherwise = ""
      z | mod s 7 == 0 = "Plong" | otherwise = ""
