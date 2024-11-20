import Euler (uniqueList)
import Control.Monad (guard)

subsets [] = [([], [])]
subsets (a:as) = do
    (fs, sc) <- subsets as
    [(a:fs, sc), (fs, a:sc)]

allResults :: [Int] -> [Rational]
allResults [] = []
allResults [a] = [toRational a]
allResults nums = do
    let allTrue _ _ = True
    let divisible a b = b /= 0
    (op, guardCond) <- [((+), allTrue), ((-), allTrue), ((*), allTrue), ((/), divisible)]
    --len <- [1..length nums - 1]
    --let fs = take len nums
    --let sc = drop len nums
    (fs, sc) <- subsets nums
    guard $ fs /= [] && sc /= []
    fresult <- allResults fs
    sresult <- allResults sc
    guard $ guardCond fresult sresult
    return $ op fresult sresult

all4 = do
    a <- [0..9]
    b <- [0..9]
    guard $ b > a
    c <- [0..9]
    guard $ c > b
    d <- [0..9]
    guard $ d > c
    return [a, b, c, d]


consecutiveStart ls = length $ takeWhile id $ zipWith (==) [1..] vals
    where
        vals = dropWhile (<= 0) $ uniqueList $ filter isInt ls
        isInt x = x == fromIntegral (round x)

explore = map (\ls -> (consecutiveStart $ allResults ls, ls)) all4

main = do
    print $ length $ subsets [1..4]
    --print all4
    print $ uniqueList $ allResults [1..4]
    print $ consecutiveStart $ allResults [1..4]
    print $ consecutiveStart [5, 4, 2, 3, 1, 1, 6]
    print $ maximum explore
