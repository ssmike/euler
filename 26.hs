base = 10

makeSimple = divide 2 . divide 5
     where
        divide divisor num  =
            if num `mod` divisor /= 0
                then num
                else divide divisor (num `div` divisor)


checkSimple 1 = 0
checkSimple n = (+1) . length $ takeWhile (/=1) series
    where
        series = iterate (multiply base) (base `mod` n)
        multiply a b = (a * b) `mod` n
        
check = checkSimple . makeSimple

main = do
    print $ makeSimple 2
    print $ maximum [(check x, x) | x <- [1..1000]]
