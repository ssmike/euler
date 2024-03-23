import Euler (primesGen)

limit = 10^6

main = do
    let nums = scanl1 (*) primesGen
    let upToLim = takeWhile (<=limit) nums
    print upToLim
