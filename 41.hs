import Euler (allPermutes, primeSqrtCheck, makeNumber)

isPrime = primeSqrtCheck

maxNumberFor n = filter isPrime $ map makeNumber $ allPermutes [1..n]
maxNumbers = map maxNumberFor [1..9]

main = do
    print $ zip [1..] maxNumbers
