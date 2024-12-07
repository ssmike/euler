import Euler (primesArray)
import Data.Array ((!))
import Control.Monad (forM_)
import System.Environment as E


chains :: [Integer] -> Integer -> [[Integer]]
chains primes limit = map (2:) $ []: chainsFrom 2 (dropWhile (<=2) primes)
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

main = do
    args <- E.getArgs
    let limit = read (head args) :: Integer
    let primesA = primesArray limit
    let primes = filter (primesA !) [1..limit]
    let check = checkList (primesA !)
    let filtered = map product $ filter check (chains primes limit)

    print $ sum filtered
    forM_ (take 10 filtered) print
    forM_ (take 10 $ chains primes limit) $ \x -> print (x, check x)
