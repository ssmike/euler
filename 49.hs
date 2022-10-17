import Data.List
import Euler (digits, primesGen, primeSqrtCheck)
import Control.Monad (forM_)

lst = [1487, 4817, 8147]


check start step = eqList seqDigits
    where
        seq = take 3 $ iterate (+step) start
        seqDigits = map (sort . (`digits` 10)) seq
        eqList (x:xs) = all (==x) xs

explore = do
    start <- takeWhile (<9999) primesGen
    step <- [1..(9999-start)`div`2]
    let seq = take 3 $ iterate (+step) start
    [seq | check start step, all primeSqrtCheck seq]


main = do
    print $ check 1487 3330
    forM_ explore print
