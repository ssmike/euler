import Control.Monad (forM_)
import Euler (mergeSorted, primeSqrtCheck, millerRabinTest)
import qualified System.Random as R
import Data.List (tails, splitAt)

diags = [
    [1, 3, 13, 31],
    [1, 5, 17, 37],
    [1, 7, 21, 43],
    [1, 9, 25, 49]]

side = [1, 3, 5, 7]

checkF f vals = vals == map f [0..length vals - 1]
sideLen x = 2 * x + 1
alls = [1, 5, 9]
allsCnt x = 4 * x + 1

diag1 x = diag2 x - sideLen x + 1
diag2 x = diag3 x - sideLen x + 1
diag3 x = diag4 x - sideLen x + 1
diag4 x = (2*x + 1)^2

--isPrime = primeSqrtCheck


merged :: [Int]
merged = foldl mergeSorted [] $ map (\x -> map x [1..]) [diag1, diag2, diag3, diag4]

portions = 1:repeat 4

fractions isPrime = fractionsFrom merged portions (0, 0)
    where
        primeVal x = if isPrime x then 1 else 0 
        fractionsFrom ms (c:cs) (cnt, prime) = newFrac : fractionsFrom tailm cs newFrac
            where
                (headm, tailm) = splitAt c ms
                newFrac = (cnt+c, prime + sum (map primeVal headm))

genNumbers :: IO [Int]
genNumbers = R.randoms . R.mkStdGen <$> R.randomIO

main = do
    primeCandidates <- take 40 `fmap` genNumbers
    let isPrime = primeSqrtCheck

    print $ checkF sideLen side
    print $ checkF diag4 $ diags !! 3
    print $ checkF diag3 $ diags !! 2
    print $ checkF diag2 $ diags !! 1
    print $ checkF diag1 $ diags !! 0
    print $ checkF allsCnt alls
    print $ take 10 $ merged
    print $ take 10 portions
    print $ take 10 $ fractions isPrime
    let explore = zip (map sideLen [1..]) $ takeWhile (\(a, b) -> b * 10 >= a) $ fractions isPrime
    forM_ explore print

    --print $ a 3 13
    --print $ b 3 13
