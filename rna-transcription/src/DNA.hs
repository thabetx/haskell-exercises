module DNA (toRNA) where

sub :: Char -> Char
sub 'G' = 'C'
sub 'C' = 'G'
sub 'T' = 'A'
sub 'A' = 'U'
sub _ = '?'

toRNA :: String -> Maybe String
toRNA s
    | not $ any (== '?') result = Just result
    | otherwise = Nothing
    where result = map sub s
