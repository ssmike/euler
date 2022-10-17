import Euler (primesGen, subtractSorted, mergeSorted)
import Control.Monad (forM_)


mergedLists = mergedListsFrom 1
    where
        mergedListsFrom n = 2*n^2+firstPrime: mergeSorted (listFrom n) (mergedListsFrom (n+1))
        listFrom n = map (+(2*n^2)) primesTail
        firstPrime = head primesGen
        primesTail = drop 1 primesGen


odds = map (\n -> n * 2 - 1) [1..]
oddComposites = subtractSorted odds primesGen

result = subtractSorted oddComposites mergedLists

main = do
    print $ take 20 odds
    print $ take 20 oddComposites
    print $ take 20 result
