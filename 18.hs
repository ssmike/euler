import qualified Data.Text.Lazy as L

unpackNumbers s = map (map readNumber) numbers
    where
        readNumber = (read :: String -> Int) . L.unpack
        numbers = filter (/= [L.pack ""]) $ map (L.split (==' ')) lines
        lines = L.split (=='\n') (L.pack s)

maxPath lines = maximum $ foldl calcPath [0] lines
    where
        calcPath paths weights = zipWith (+) weights $ zipWith max first second
            where
                first = paths ++ [last paths]
                second = head paths : paths


main = do
    lines <- readFile "triangle"
    print $ maxPath $ unpackNumbers lines
