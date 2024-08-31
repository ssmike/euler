import Euler (digits)


integralSqrt :: Integer -> Integer
integralSqrt n = lookForBound 0 (n + 1)
    where
        lookForBound l r
            | l + 1 >= r = l
            | m * m > n = lookForBound l m
            | otherwise = lookForBound m r
            where m = (l + r) `div` 2
            

digits100 x = sum digs
    where 
        integralDigits = integralSqrt ( 10^200 * x) `digits` 10
        digs = take 100 integralDigits

issquare x = m * m == x
    where m = integralSqrt x

main = do
    print $ digits100 2
    print $ map integralSqrt [1..10]
    print $ filter (not . issquare) [1..100]
    print $ sum $ map digits100 $ filter (not . issquare) [1..100]
    
