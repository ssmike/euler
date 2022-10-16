import Data.Array

lim = 100

sumCount = array ((0, 0), (lim, lim)) [((i, j), count i j) | i <- [0..lim], j <- [0..lim]]
    where 
        count 0 0 = 1
        count _ 0 = 0
        count i j = sum [sumCount ! (i - steps * j, j - 1) | steps <- [0..(i `div` j)+1], i >= steps * j]

sumWays n = sumCount ! (n, n - 1)

main = do
    print $ sumWays 2 
    print $ sumWays 5
    print $ sumWays 100
