module SecretHandshake (handshake) where

import Data.Bits

rev :: Int -> [String] -> [String]
rev a l
  | (.&.) a 16 == 16 = reverse l
  | otherwise = l

jump :: Int -> [String] -> [String]
jump a l
  | (.&.) a 8 == 8 = rev a $ l ++ ["jump"]
  | otherwise = rev a l

close :: Int -> [String] -> [String]
close a l
  | (.&.) a 4 == 4 = jump a $ l ++ ["close your eyes"]
  | otherwise = jump a l

double :: Int -> [String] -> [String]
double a l
  | (.&.) a 2 == 2 = close a $ l ++ ["double blink"]
  | otherwise = close a l

wink :: Int -> [String] -> [String]
wink a l
  | (.&.) a 1 == 1 = double a $ l ++ ["wink"]
  | otherwise = double a l

handshake :: Int -> [String]
handshake a = wink a []
