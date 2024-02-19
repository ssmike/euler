import Euler (allPermutes)
import Control.Monad (guard)

lim = 9999
gen3 = filter (<=lim) $ [n*(n + 1) `div` 2 | n <- [1..lim]]
gen4 = filter (<=lim) $ [n*n | n <- [1..lim]]
gen5 = filter (<=lim) $ [n*(3 * n - 1) `div` 2 | n <- [1..lim]]
gen6 = filter (<=lim) $ [n*(2 * n - 1) | n <- [1..lim]]
gen7 = filter (<=lim) $ [n*(5 * n - 3) `div` 2 | n <- [1..lim]]
gen8 = filter (<=lim) $ [n*(3 * n - 2) | n <- [1..lim]]

findPairsWithPlan :: Int -> [[Int]] -> [[Int]]

findPairsWithPlan seed [] = [[seed]]

findPairsWithPlan seed plan = do
    edge <- head plan
    guard $ (edge `mod` 100) == seed
    let next = edge `div` 100
    guard $ next >= 10
    map (seed:) $ findPairsWithPlan next (tail plan)

findPairs = do
    gens <- allPermutes [gen3, gen4, gen5, gen6, gen7, gen8]
    seed <- [10..99]
    pairs <- findPairsWithPlan seed gens
    guard $ last pairs == seed
    return $ tail pairs

calcSums seq = zipWith (\a b -> a * 100 + b) seq seq1
    where
        seq1 = tail seq ++ [head seq]


main = do
    print $ take 10 gen3
    print $ take 10 gen4
    print $ take 10 gen5
    print $ take 10 gen6
    print $ take 10 gen7
    print $ take 10 gen8
    print $ length gen3
    print $ length gen4
    print $ length gen5
    print $ length gen6
    print $ length gen7
    print $ length gen8
    print findPairs
    let numbers = calcSums $ head findPairs
    print numbers
    print $ sum numbers

