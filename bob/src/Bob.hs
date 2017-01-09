module Bob (responseFor) where

import Data.Char
import Data.List

isUpperOrNotAlpha :: Char -> Bool
isUpperOrNotAlpha x = isUpper x || isAlpha x
isAllUpper :: String -> Bool
isAllUpper x = all isUpperOrNotAlpha x

isQuestion :: String -> Bool
isQuestion x = last x == '?'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

responseFor :: String -> String
responseFor x = answer (trim x)

answer :: String -> String
answer x
      |  length x == 0  = "Fine. Be that way!"
      |  not (any isAlpha x)  && not (isQuestion x) = "Whatever."
      |  isQuestion x && not (any isUpper x) = "Sure."
      |  isAllUpper x = "Whoa, chill out!"
      |  isQuestion x = "Sure."
      |  otherwise = "Whatever."

