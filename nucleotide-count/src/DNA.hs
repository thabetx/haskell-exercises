module DNA (count, nucleotideCounts) where

import qualified Data.Map as Map
import qualified Data.List as List

invalidNucleotide :: Char -> Bool
invalidNucleotide nucleotide = not $ elem nucleotide "ACGT"

count :: Char -> String -> Either String Int
count nucleotide strand
    | invalidNucleotide nucleotide || any (invalidNucleotide) strand = Left "invalid"
    | otherwise = Right . length . filter (==nucleotide) $ strand

nucleotideCounts :: String -> Either String (Map.Map Char Int)
nucleotideCounts strand
    | any (invalidNucleotide) strand = Left "invalid"
    | otherwise = Right . Map.fromList .
                  map ( \segment@(x:xs) -> (x, length segment - 1) ) .
                  List.group . List.sort $ strand ++ "ACGT"
