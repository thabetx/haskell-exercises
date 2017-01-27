{-simple functions-}
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
isEven x = (x`mod`2)==0

{-guards and where-}
areWeThereYet :: (RealFloat a) => a -> a -> String
areWeThereYet distance speed
    | remainingTime <= short = "almost there"
    | remainingTime <= long  = "not yet"
    | otherwise              = "..."
    where remainingTime = distance / speed
          short = 10
          long = 1000

{-maybe-}
safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0     = Just (log x)
    | otherwise = Nothing

{-foldl-}
grades :: School -> [Int]
grades school = sort . nub $ foldl (\acc x -> acc ++ [fst x]) [] $ students school

{-pattern matching-}
accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = [f x] ++ accumulate f xs
