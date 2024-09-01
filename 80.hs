import Euler (digits, integralSqrt)


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
    
