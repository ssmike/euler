module Euler where

import qualified Data.Array.ST as S
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.List as L
import Control.Monad (forM_, when, guard)
import Data.Array (array, (!))

uniqueSorted list = map head $ L.group list
uniqueList list = uniqueSorted $ L.sort list

allPermutes :: [a] -> [[a]]
allPermutes [] = [[]]
allPermutes xs = do
    (index, value) <- zip [0..] xs
    let (before, after) = splitAt index xs
    (value:) <$> allPermutes (before ++ tail after)

divisorsUpTo limit = S.runSTArray $ do
    anyDiv <- S.newArray (2, limit) 0
    lastPrime <- newSTRef 2
    forM_ [2..limit] $ \i -> do
        curDiv <- S.readArray anyDiv i
        when (curDiv == 0) $ do
            S.writeArray anyDiv i i
            readSTRef lastPrime >>= \x -> S.writeArray anyDiv x i
            writeSTRef lastPrime i
        curDiv <- S.readArray anyDiv i
        
        let checkMuls p = do
                next <- S.readArray anyDiv p
                if next > p && i * next <= limit && next <= curDiv
                    then checkMuls next >>= (return . (p:))
                    else return [p]

        checkedMuls <- checkMuls 2
        forM_ checkedMuls $ \mul -> do
            let num = mul * i
            when (num <= limit) $ do
                S.writeArray anyDiv num mul
    return anyDiv

multiplesBy primeDivisors 1 = []
multiplesBy primeDivisors x = (firstDiv, power): multiplesBy primeDivisors remainder
    where 
        firstDiv = let val = primeDivisors ! x in min val x 
        (power, remainder) = divBy firstDiv x

        divBy x n
            | n `mod` x /= 0 = (0, n)
            | otherwise = let (pow, val) = divBy x (n `div` x)
                          in (pow + 1, val)

phis n = mphi
    where
        mphi = array (2, n) [(i, phi i) | i <- [2..n]]
        minDivisor = divisorsUpTo n

        divBy n p
            | n `mod` p /= 0 = (n, 1) 
            | otherwise = (nr, pr * p)
                where (nr, pr) = divBy (div n p) p

        phi n
            | p >= n = n - 1
            | a == 1 = n - (n `div` p)
            | otherwise = (mphi ! a) * (mphi ! b)
                where
                    p = minDivisor ! n
                    (a, b) = divBy n $ minDivisor ! n


multiples :: Integer -> [(Integer, Int)]
multiples = multiplesFrom 2
    where
        divBy x n
            | n `mod` x /= 0 = (0, n)
            | otherwise = let (pow, val) = divBy x (n `div` x)
                          in (pow + 1, val)

        multiplesFrom x n
            | n == 1 = []
            | n `mod` x == 0 =
                let (pow, val) = divBy x n
                in (x, pow): multiplesFrom (x + 1) val
            | x * x >= n = [(n, 1)]
            | otherwise = multiplesFrom (x + 1) n


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
    let aLimit = round $ sqrt $ fromIntegral perimeterLimit
    a <- [1..aLimit+1]
    b <- [1..a-1]
    let fst = 2*a*b
    let snd = a^2-b^2
    let third = a^2+b^2
    let perimeter = fst+snd+third
    guard $ gcd a b == 1
    [(fst*mul, snd*mul, third*mul) | mul <- [1..div perimeterLimit perimeter]]

makeNumber :: Num a => [a] -> a
makeNumber = foldl (\a b -> a * 10 + b) 0

primeSqrtCheck n = all (\x -> n `mod` x /= 0) candidates
    where
        candidates = takeWhile (\x -> x^2 <= n) [2..]

multiplyByMod n a b = (a*b) `mod` n

powerByMod _ 0 _ = 1
powerByMod n a p
    | p == 1 = a`mod`n
    | p `mod` 2 == 1 = a `modMul` powerByMod n a (p - 1)
    | otherwise = powerByMod n (modMul a a) (p `div` 2)
    where
        modMul a b = (a*b) `mod` n

data RemaindersRing integral = RemainderClass {_remainder :: integral, _quotient :: !integral}
    deriving Show

remainderBy p x = RemainderClass {_remainder = x `mod` p, _quotient = p}
remainderValue = _remainder

instance (Integral integral) => Num (RemaindersRing integral) where
    negate a = a {_remainder = _quotient a - _remainder a}
    (+) a b = RemainderClass {_remainder = (_remainder a + _remainder b) `mod` quotient , _quotient = quotient}
        where
            quotient = max (_quotient a) (_quotient b)
    (*) a b = RemainderClass {_remainder = (_remainder a * _remainder b) `mod` quotient , _quotient = quotient}
        where
            quotient = max (_quotient a) (_quotient b)
    fromInteger x = RemainderClass (fromInteger x) 0

    abs = id
    signum = id


millerRabinTest :: [Integer] -> Integer -> Bool
millerRabinTest _ 0 = False
millerRabinTest _ 1 = False
millerRabinTest withnesses n = all (checkPrimeWith . (`mod`n)) withnesses
    where
        modMul = multiplyByMod n
        modPow = powerByMod n


        twoMultiples x
            | x `mod` 2 == 1 = [x]
            | otherwise = 2:twoMultiples (x `div` 2)

        checkPrimeWith 0 = True
        checkPrimeWith withness
            | head powers == 1 = True
            | last powers /= 1 = False
            | (n-1) `elem` powers = True
            | otherwise = False
            where
                (basePower:multiples) = reverse $ twoMultiples (n - 1)
                base = modPow withness basePower
                powers = scanl modPow base multiples


intersectSorted [] xs = []
intersectSorted _ [] = []
intersectSorted (x:xs) (y:ys)
    | x > y = intersectSorted (y:ys) (x:xs)
    | x < y = intersectSorted xs (y:ys)
    | otherwise = x:intersectSorted xs ys

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] xs = xs
mergeSorted xs [] = xs
mergeSorted allx@(x:xs) ally@(y:ys)
    | x < y = x:mergeSorted xs ally
    | x == y = x:mergeSorted xs ys
    | otherwise = y:mergeSorted allx ys

subtractSorted xs [] = xs
subtractSorted [] xs = []
subtractSorted allx@(x:xs) ally@(y:ys)
    | x < y = x:subtractSorted xs ally
    | x == y = subtractSorted xs ys
    | otherwise = subtractSorted allx ys
