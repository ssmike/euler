import qualified Data.Array.ST as S
import Control.Monad (forM_, when)
import Data.Array

numLen :: Int -> Int
numLen 0 = 0
numLen n = 1 + numLen (n `div` 10)

allCuts n = filter (/=0) $ lefts ++ rights
    where
        len = numLen n
        powers = [10^k | k <- [0..len]]
        lefts = mod <$> [n] <*> powers
        rights = div <$> [n] <*> powers

primesLim = 10^6
primeBits = S.runSTArray $ do
    isprime <- S.newArray (1, primesLim) True
    S.writeArray isprime 1 False
    forM_ [2..primesLim] $ \i -> do
        checkPrime <- S.readArray isprime i
        when  checkPrime $
            forM_ (takeWhile (<primesLim) (iterate (+i) (i*i))) $
                \j -> S.writeArray isprime j False
    return isprime

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
