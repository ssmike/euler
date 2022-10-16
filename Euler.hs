module Euler where

import qualified Data.Array.ST as S
import Control.Monad (forM_, when)

allPermutes [] = [[]]

allPermutes list = list >>= permutesFrom
    where
        permutesFrom n = map (n:) $ allPermutes withoutn
            where withoutn = filter (/=n) list

primesArray limit = S.runSTArray $ do
    isprime <- S.newArray (1, limit) True
    S.writeArray isprime 1 False
    forM_ [2..limit] $ \i -> do
        checkPrime <- S.readArray isprime i
        when checkPrime $
            forM_ (takeWhile (<limit) (iterate (+i) (i*i))) $
                \j -> S.writeArray isprime j False
    return isprime

primesGen = primesFrom [2..]
    where
        primesFrom (n:ns) = n:primesFrom (filter (\s -> s `mod` n /= 0) ns)

factorial n = product [1..n]

numLength :: Int -> Int
numLength 0 = 0
numLength n = 1 + numLength (n `div` 10)

allRotations n = n: takeWhile (/=n) rotations
    where
        rotations = drop 1 $ iterate (rotate $ numLength n)  n
        rotate len n = digit * 10^(len-1) + (n`div`10)
            where
                digit = n `mod` 10

palyndrom x = x == reverse x

digits n base = reverse $ digits_ n base
    where
        digits_ 0 _ = []
        digits_ n base = n`mod`base : digits_ (n`div`base) base

pithagoreans perimeterLimit = do
    a::Int <- [1..perimeterLimit]
    b::Int <- [1..a-1]
    let fst = 2*a*b
    let snd = a^2-b^2
    let third = a^2+b^2
    let perimeter = fst+snd+third
    [(fst*mul, snd*mul, third*mul) |
        gcd a b == 1, mul <- [1..div perimeterLimit perimeter]]

makeNumber :: [Int] -> Int
makeNumber = foldl (\a b -> a * 10 + b) 0

primeSqrtCheck n = all (\x -> n `mod` x /= 0) candidates
    where
        candidates = takeWhile (\x -> x^2 < n) [2..]
