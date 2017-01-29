module Phone (areaCode, number, prettyPrint) where

import Data.Char
import Data.Maybe

areaCode :: String -> Maybe String
areaCode input
    | isJust filtered = Just $ take 3 $ fromJust filtered
    | otherwise = Nothing
    where filtered = number input

number :: String -> Maybe String
number input
    | length filtered == 10 = Just filtered
    | length filtered == 11 && head filtered == '1' = Just $ tail filtered
    | otherwise = Nothing
    where filtered = filter isDigit input

prettyPrint :: String -> Maybe String
prettyPrint input
    | isJust filtered = Just $ "(" ++ prefix ++  ") " ++ middle ++ "-" ++ suffix
    | otherwise = Nothing
    where filtered = number input
          extracted = fromJust filtered
          prefix = take 3 extracted
          middle = take 3 $ drop 3 extracted
          suffix = drop 6 extracted
