import qualified Data.Array.ST as S
import Control.Monad (forM_, when)
import Data.Array
import Euler (primesArray, numLength)

allCuts n = filter (/=0) $ lefts ++ rights
    where
        len = numLength n
        powers = [10^k | k <- [0..len]]
        lefts = mod <$> [n] <*> powers
        rights = div <$> [n] <*> powers

primesLim = 10^6
primeBits = primesArray primesLim

primes = filter (primeBits!) [1..primesLim]

checkPrime = all (primeBits!) . allCuts

main = do
    print $ allCuts 3797
    print $ checkPrime 3797
    let checked = filter checkPrime primes
    print checked
    print $ length checked
    print $ sum $ filter (>10) checked
    --print $ length $ takeWhile (<100) checked
