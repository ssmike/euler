import Euler (primesGen, allPermutes, makeNumber, digits)
import Data.List (tails)

primes = map fromIntegral $ take 7 primesGen
digitsPermutes = allPermutes [0..9]

--all3s :: [Int] -> [[Int]]
all3s = drop 1 . filter ((==3).length) . map (take 3) . tails

check permute = and (zipWith (\slice num -> makeNumber slice `mod` num == 0) (all3s permute) primes)

main = do
    print $ all3s $ digits 1406357289 10
    print $ check $ digits 1406357289 10
    let pans = filter check digitsPermutes
    print $ sum $ map makeNumber pans
