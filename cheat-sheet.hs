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
