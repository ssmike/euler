
normFrac (a, b) = (a `div` g, b `div` g)
    where g = gcd a b

lowerBoundWith (a, b) denom = normFrac ((a * denom - 1) `div` b, denom)

lessFrac (a, b) (c, d) = a * d < b * c

maxFrac a b = if lessFrac a b then b else a

main = do
    print $ lowerBoundWith (3, 7) 5
    print $ map (lowerBoundWith (3, 7)) [1..8]
    print $ foldl1 maxFrac $ map (lowerBoundWith (3, 7)) [1..8]
    print $ foldl1 maxFrac $ map (lowerBoundWith (3, 7)) [1..1000000]
