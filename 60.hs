import Euler (primeSqrtCheck, digits, makeNumber, primesGen, millerRabinTest)
import Control.Monad (guard, forM_)
import Data.Array (array, (!), Array)
import qualified System.Random as R

--isPrime = primeSqrtCheck

concatNum a b = makeNumber $ digits a 10 ++ digits b 10
checkPair isPrime a b = isPrime (concatNum a b) && isPrime (concatNum b a)

checkCollection _ [] = True
checkCollection isPrime (x:xs) =  and $ checkCollection isPrime xs: map (checkPair isPrime x) xs

explore _ 0 x = return x
explore isPrime n [] = do
    a0 <- primesGen
    explore isPrime (n - 1) [a0]
explore isPrime n alla@(a0:as) = do
    an <- takeWhile (<a0) primesGen
    guard $ all (checkPair isPrime an) alla
    explore isPrime (n - 1) (an:alla)


memoExplore isPrime n = memo ! n
    where
        memo :: Array Int [[Integer]]
        memo = array (0, n) [(i, explore i) | i <- [0..n]]
        explore :: Int -> [[Integer]]
        explore 0 = [[]]
        explore n = do
            an <- primesGen
            rest <- takeWhile (all (<an) . take 1) $ memo ! (n - 1)
            guard $ all (checkPair isPrime an) rest
            return $ an:rest


genNumbers = R.randoms . R.mkStdGen <$> R.randomIO

main = do
    primeCandidates <- take 40 `fmap` genNumbers
    let isPrime = millerRabinTest primeCandidates
    print $ checkCollection isPrime ([3, 7, 109, 673] :: [Integer])
    --forM_ (explore isPrime 4 []) $ \x -> print (sum x, x)
    forM_ (memoExplore isPrime 5) $ \x -> print (sum x, x)
