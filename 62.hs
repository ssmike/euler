import Euler (digits, makeNumber, allPermutes, uniqueList)
import Control.Monad (forM_, join)
import Data.List (sort, group, maximumBy, groupBy, sortBy)
import Data.Ord (comparing, Down)

isCube x = (==x) . head $ dropWhile (<x) candidates
    where
        lowerBound = floor $ (**(1/3)) $ fromIntegral x
        candidates = map (^3) [lowerBound..]

cubes x = length $ filter isCube $ uniqueList all
    where
        digs = digits (x^3) 10
        all = map makeNumber $ allPermutes digs

start = 499
limit = 100000

numProfile x = makeNumber . reverse . sort $ digits x 10

groups = groupBy (\(a, ap) (b, bp) -> ap == bp)  $ sortBy (comparing snd) $ zip [1..limit] $ map (\x -> numProfile (x^3)) [1..limit]


main = do
    -- maximumBy (comparing length)
    print $ minimum $ map fst $ join $ filter ((==5) . length) groups
    --print $ (map makeNumber $ allPermutes $ digits 988776654432110 10)
    --print $ cubes 345
    --forM_ (zip [start..] (scanl1 (||) $ map ((==5) . cubes) [start..])) print
    --forM_ (group $ map numProfile [1..limit]) print
    

