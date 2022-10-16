import Data.Array
import qualified Data.Array.ST as S
import Control.Monad (forM_, when)

primesLim = n^2 + n*lim + lim
    where 
        n = lim
        lim = 1000


--primebits = primebitsfrom 1 primes
--    where
--        primebitsfrom cur plist@(p:ps) = if cur < p then False: primebitsfrom (cur + 1) plist else True: primebitsfrom (cur+1) ps
--
--        primes = primesFrom [2..]
--        primesFrom (n:ns) = n:primesFrom (filter (\s -> s `mod` n /= 0) ns)
--primesA = listArray  (1, primesLim) (take primesLim primebits)

primes = S.runSTArray $ do
    isprime <- S.newArray (1, primesLim) True
    S.writeArray isprime 1 False
    forM_ [2..primesLim] $ \i -> do
        checkPrime <- S.readArray isprime i
        when checkPrime $
            forM_ (takeWhile (<primesLim) (iterate (+i) (i*i))) $
                \j -> S.writeArray isprime j False
    return isprime
    


checkPrime n = n > 0 && (primes ! n)

searchPolynomial lim = do
    a <- [-lim..lim]
    b <- [-lim..lim]
    let primeTests = map (\n -> checkPrime $ n^2+a*n+b) [1..lim]
    [(length $ takeWhile id primeTests, (a, b)) | checkPrime b]


main = do
    print $ filter checkPrime [1..100]
    print $ maximum $ searchPolynomial 1000
