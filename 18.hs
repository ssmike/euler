maxPath lines = maximum $ foldl calcPath [0] lines
    where
        calcPath paths weights = zipWith (+) weights $ zipWith max first second
            where
                first = paths ++ [last paths]
                second = head paths : paths


main = do
    fileStr <- readFile "triangle"
    print $ maxPath $ map (map (read::String->Int) . words) $ lines fileStr
