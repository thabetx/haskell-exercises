module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode [] = ""
decode (x:y:xs)
          | isNumber x = replicate (read ([x]) ::Int) y ++ decode xs
          | otherwise =  x : decode (y:xs)
decode (x:_) = [x]

solve :: Int -> Char -> String -> String
solve cnt prev (x : xs)
          | prev == x = solve (cnt+1) x xs
          | otherwise = show(cnt)++[prev]++solve 1 x xs
solve cnt prev _ = show(cnt)++[prev]

encode :: String -> String
encode x = filter (/='1')  (drop 2 (solve 1 '?' x))

