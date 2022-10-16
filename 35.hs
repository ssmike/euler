import Euler
import qualified Data.Array.ST as S
import Control.Monad (forM_, when)
import Data.Array

primesLim = 10^6
primeBits = primesArray primesLim

primes = filter (primeBits!) [1..primesLim]

checkPrime = all (primeBits!) . allRotations

main = do
    let checked = filter checkPrime primes
    print checked
    print $ length checked
    print $ length $ takeWhile (<100) checked
