module Hamming (distance) where

import Data.List

distance :: String -> String -> Maybe Integer
distance strand1 strand2
    | length strand1 == length strand2 = Just $ genericLength $ filter (\(a, b) -> a/=b) $ zip strand1 strand2
    | otherwise = Nothing
