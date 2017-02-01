module Scrabble (scoreLetter, scoreWord) where

import Data.Char

scoreLetter :: Char -> Int
scoreLetter letter
    | elem x "AEIOULNRST" = 1
    | elem x "DG"         = 2
    | elem x "BCMP"       = 3
    | elem x "FHVWY"      = 4
    | elem x "K"          = 5
    | elem x "JX"         = 8
    | elem x "QZ"         = 10
    | otherwise           = 0
    where
      x = toUpper letter

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter
