import Data.Array


coins = [1, 2, 5, 10, 20, 50, 100, 200]
coinsCnt = length coins

limNum = 200

coinsA = listArray (1, length coins) coins 

sumCount = array ((0, 0), (limNum, coinsCnt)) [((i, j), count i j) | i <- [0..limNum], j <- [0..coinsCnt]]
    where 
        count 0 0 = 1
        count _ 0 = 0
        count i j = sum [sumCount ! (i - steps * p, j - 1) | steps <- [0..(i `div` p)+1], i >= steps * p]
            where p = coinsA ! j

sumWays n = sumCount ! (n, coinsCnt)

main = do
    print [1..20]
    print $ map sumWays [1..20]
    print $ sumWays 200
