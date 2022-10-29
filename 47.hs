import Euler (primesArray)
import qualified Data.Array.ST as S
import Control.Monad (forM_, when)
import Data.List (tails)
import Data.Array

limit = 10^6

factorsNumberA = S.runSTArray $ do
    primeFactors <- S.newArray (1, limit) 0
    S.writeArray primeFactors 1 0
    forM_ [2..limit] $ \i -> do
        factors <- S.readArray primeFactors i
        when (factors == 0) $
            forM_ (takeWhile (<limit) (iterate (+i) i)) $ \j -> do
                factors <- S.readArray primeFactors j
                S.writeArray primeFactors j (factors + 1)
    return primeFactors

factorsNumber :: [Int]
factorsNumber = map (factorsNumberA!) [1..limit]

allnths :: Int -> [a] -> [[a]]
allnths n list = map (take n) $ tails list

find n = filter ((==replicate n n) . snd) $ zip [1..limit] (allnths n factorsNumber)


main = do
    --print $ map (factorsNumberA!) [14, 15, 644, 645, 646]
    --print $ find 3
    print $ find 4
