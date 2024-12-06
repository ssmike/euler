import Euler (primesArray, millerRabinTest)
import Data.Array ((!))
import Control.Monad (forM_)
import qualified System.Random as R


limit = 10^8
primes = filter (primesArray limit !) [1..limit]


chains :: [[Integer]]
chains = map (2:) $ chainsFrom 2 (dropWhile (<=2) primes)
    where
        chainsFrom _ [] = []
        chainsFrom acc (p:primes)
            | acc * p > limit = []
            | otherwise = [p] : (with ++ without)
                where
                    with = map (p:) $ chainsFrom (acc*p) primes
                    without = chainsFrom acc primes
                

alldivs [] = [(1, 1)]
alldivs (x:xs) = do
    (am, bm) <- [(1, x), (x, 1)]
    (a, b) <- alldivs xs
    return (am * a, bm * b)


checkList isprime ls = all (isprime . uncurry (+)) (alldivs ls)

genNumbers = R.randoms . R.mkStdGen <$> R.randomIO

main = do
    primeCandidates <- take 40 `fmap` genNumbers
    let isPrime = millerRabinTest primeCandidates
    let check = checkList isPrime
    let filtered = map product $ filter check chains

    print $ sum filtered
    -- forM_ filtered print
