import Control.Monad (guard)
allResults :: [Int] -> [Int]
allResults [] = []
allResults [a] = [a]
allResults nums = do
    let allTrue _ _ = True
    let divisible a b = 0 == mod a b
    (op, guardCond) <- [((+), allTrue), ((-), allTrue), ((*), allTrue), (div, divisible)]
    len <- [1..length nums - 1]
    let fs = take len nums
    let sc = drop len nums
    fresult <- allResults fs
    sresult <- allResults sc
    guard $ guardCond fresult sresult
    return $ op fresult sresult

all4 = do
    a <- [0..9]
    b <- [0..9]
    c <- [0..9]
    d <- [0..9]
    return [a, b, c, d]

explore = do
    s4 <- all4
    let ress = allResults s4


main = do
    print all4
    print $ allResults [1..4]
