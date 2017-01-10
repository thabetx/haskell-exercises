module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode [] = ""
decode x = replicate (cnt num_string) next ++ decode (tail rest)
           where num_string = takeWhile isNumber x
                 cnt :: String -> Int
                 cnt s
                  | s =="" = 1
                  | otherwise = read s ::Int
                 rest = dropWhile isNumber x
                 next = head rest

solve :: Int -> Char -> String -> String
solve cnt prev (x : xs)
          | prev == x = solve (cnt+1) x xs
          | cnt > 1 = show(cnt)++[prev]++solve 1 x xs
          | otherwise = [prev]++solve 1 x xs
solve cnt prev _
          | cnt > 1 = show(cnt)++[prev]
          | otherwise = [prev]


encode :: String -> String
encode x = tail (solve 1 '?' x)

