import Euler (primesArray, uniqueList)
import Data.Array
import Control.Monad (forM_)
import Data.List (tails)

lim = 10^6

primesA = primesArray lim
primes = filter (primesA!) [1..lim]

primeSegms = concatMap (zip [1..] . takeWhile (<=lim) . scanl1 (+)) $ tails primes 
primePrimeSegms = filter ((primesA!) . snd) primeSegms

main = do
    print $ maximum primePrimeSegms
