module Raindrops (convert) where

convert s
    | (mod s 3 > 0) && (mod s 5 > 0) && (mod s 7 > 0) = show s
    | otherwise = x s ++ y s ++ z s
    where
      x s | mod s 3 == 0 = "Pling" | otherwise = ""
      y s | mod s 5 == 0 = "Plang" | otherwise = ""
      z s | mod s 7 == 0 = "Plong" | otherwise = ""
