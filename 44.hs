import Control.Monad (forM_)

pentagonalsFrom n = map (\n -> (n * (3 * n - 1)) `div` 2) [n..]
pentagonals = pentagonalsFrom 1

isPentagonal 1 = True
isPentagonal n = bound == n
    where 
        bound = head $ dropWhile (<n) nums
        nums = pentagonalsFrom $ floor $ sqrt $ (2 * fromIntegral n)/3

findDiffs = do
    fs <- pentagonals
    sc <- takeWhile (<fs) pentagonals
    let diff = fs - sc
    let sm = fs + sc
    [diff | isPentagonal diff && isPentagonal sm]


main = do
    forM_ findDiffs print
