
selections = iterate makeSelections [1]
    where
        makeSelections row = 1:zipWith (+) row (tail row) ++ [1]


main = do
    print $ length $ filter (>(10^6)) $ concat $ take 101 selections 
