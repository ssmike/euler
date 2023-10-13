import Euler (digits)


measure = sum [length $ check x | x <- [1..9]]
    where
        check x = filter (uncurry (==)) $ takeWhile (uncurry (<=)) $ zip [1..] lens
            where
                powers = iterate (*x) x
                lens = map (length . (`digits` 10)) powers

main = do
    print measure
