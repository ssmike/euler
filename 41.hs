import Euler (allPermutes, primeSqrtCheck)

isPrime = primeSqrtCheck

makeNumber :: [Int] -> Int
makeNumber = foldl (\a b -> a * 10 + b) 0

maxNumberFor n = filter isPrime $ map makeNumber $ allPermutes [1..n]
maxNumbers = map maxNumberFor [1..9]

main = do
    print $ zip [1..] maxNumbers
