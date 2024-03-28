import Euler (divisorsUpTo, multiplesBy)
import Data.Array ((!))
import Data.Foldable (maximumBy)
import Control.Monad (forM_)
import qualified Data.Set as Set

limit = 1*10^6

divisors = divisorsUpTo limit

multiples = multiplesBy divisors


amicableOf x = product (map sumMuls $ multiples x) - x
    where
        sumMuls (p, d) = (p^(d+1) - 1) `div` (p - 1)

exploreFrom start = maybe [] chainFrom chainStart
    where
        minFrom start seen
            | start == 0 = Nothing
            | start > limit = Nothing
            | Set.member start seen = Just start
            | otherwise = minFrom (amicableOf start) (Set.insert start seen)

        chainStart = minFrom start Set.empty

        chainFrom start = (start:) $ takeWhile (\x -> x /= start && x <= limit) $ drop 1 $ iterate amicableOf start

lenFrom = length . exploreFrom

main = do
    print $ lenFrom 1064
    print $ lenFrom 12496
    let allChains = filter (/=[]) $ map exploreFrom [2..limit]
    --let allChains = map exploreFrom [2..limit]
    let keys = map (\xs -> (length xs, xs)) allChains
    let maxKey = maximum keys
    forM_ (map (minimum.snd) $ filter (\(x, y) -> x == fst maxKey) keys) print


