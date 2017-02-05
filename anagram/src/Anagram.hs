module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor input = filter (\ x -> normalize x == normalizedInput && map toLower x /= lowerCaseInput)
  where normalize = sort . map toLower
        normalizedInput = normalize input
        lowerCaseInput = map toLower input
