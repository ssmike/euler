import Euler (digits)

sumDigs x = sum $ map (^2) (digits x 10)

chain :: Integer -> [Integer]
chain x
    | x == 1 = [1]
    | x == 89 = [89]
    | otherwise = x: chain (sumDigs x)



main = do
    print $ chain 44
    print $ chain 85
    print $ filter ((==89) . last . chain) [44, 85]
    print $ length $ filter ((==89) . last . chain) [1..10^7]
