import Data.Array (array, (!))
import Euler (divisorsUpTo)

lim = 100
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
                    

main = do
    print $ phis 10
    print $ sum $ phis 4
    print $ sum $ phis 10
    print $ sum $ phis 8
    print $ sum $ phis 1000000
