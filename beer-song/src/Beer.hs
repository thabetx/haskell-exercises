module Beer (song) where

gen :: Int -> String
gen x
    | x == 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
                \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    | x == 2    = n ++ " bottles of beer on the wall, " ++ n ++
                       " bottles of beer.\nTake one down and pass it around, " ++ m ++
                       " bottle of beer on the wall.\n\n" ++ (gen $ x-1)
    | x == 1    = n ++ " bottle of beer on the wall, " ++ n ++
                       " bottle of beer.\nTake it down and pass it around," ++
                       " no more bottles of beer on the wall.\n\n" ++ (gen $ x-1)
    | otherwise = n ++ " bottles of beer on the wall, " ++ n ++
                       " bottles of beer.\nTake one down and pass it around, " ++ m ++
                       " bottles of beer on the wall.\n\n" ++ (gen $ x-1)
    where n = show x
          m = show $ x-1

song :: String
song = gen 99
