import Euler (digits, factorial, uniqueList)
import Control.Monad (forM_)
import Data.Array ((!), array, elems)

next n =  sum . map factorial $ digits n 10

nextL = uniqueList . map next

findIDPoint (x:xs)
    | x == head xs = x
    | otherwise = findIDPoint xs

chains lim ids next = chainsA
    where 
        chainsA = array (1, lim) [(i, chainLen i) | i <- [1..lim]]

        chainLen n
            | n `elem` ids = (1+) . length $ takeWhile (/=n) $ iterate next (next n)
            | otherwise = 1 + chainsA ! next n
    

main = do
    --print $ take 20 $ iterate next 69
    let lim = 1000000
    let nextLim = lim * 3
    let ids = findIDPoint $ iterate nextL [1..lim]
    print ids
    let results = chains nextLim ids next
    print $ results ! 69
    print $ length $ filter (==60) . take lim . elems $ results
