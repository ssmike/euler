import Data.Array


primes = primesFrom [2..]
    where
        primesFrom (n:ns) = n:primesFrom (filter (\s -> s `mod` n /= 0) ns)

limNum = 100
limPrimes = limNum

primesA = listArray (1, limPrimes) (take limPrimes primes)

sumCount = array ((0, 0), (limNum, limPrimes)) [((i, j), count i j) | i <- [0..limNum], j <- [0..limPrimes]]
    where 
        count 0 0 = 1
        count _ 0 = 0
        count i j = sum [sumCount ! (i - steps * p, j - 1) | steps <- [0..(i `div` p)+1], i >= steps * p]
            where p = primesA ! j

sumWays n = sumCount ! (n, limPrimes)

main = do
    print $ take 20 primes
    print $ sumWays 10
    print $ sumWays 100
    print $ sumWays 59
    print $ filter (\s -> sumWays s >= 5000) [1..limNum]

