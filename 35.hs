import qualified Data.Array.ST as S
import Control.Monad (forM_, when)
import Data.Array

numLen :: Int -> Int
numLen 0 = 0
numLen n = 1 + numLen (n `div` 10)

allRotations n = n: takeWhile (/=n) rotations
    where
        rotations = drop 1 $ iterate (rotate $ numLen n)  n
        rotate len n = digit * 10^(len-1) + (n`div`10)
            where
                digit = n `mod` 10

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

checkPrime = all (primeBits!) . allRotations

main = do
    let checked = filter checkPrime primes
    print checked
    print $ length checked
    print $ length $ takeWhile (<100) checked
