allPermutes [] = [[]]

allPermutes list = list >>= permutesFrom
    where
        permutesFrom n = map (n:) $ allPermutes withoutn
            where withoutn = filter (/=n) list


isPrime n = all (\x -> n `mod` x /= 0) candidates
    where
        candidates = takeWhile (\x -> x^2 < n) [2..]


makeNumber :: [Int] -> Int
makeNumber = foldl (\a b -> a * 10 + b) 0

maxNumberFor n = filter isPrime $ map makeNumber $ allPermutes [1..n]
maxNumbers = map maxNumberFor [1..9]

main = do
    print $ zip [1..] maxNumbers
