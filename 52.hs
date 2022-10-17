import Euler (digits)
import Data.List (sort)


check n = allEq digs
    where
        nums = map (*n) [2..6]
        digs = map (sort . (`digits`10)) nums
        allEq (x:xs) = all (==x) xs

main = print $ head $ filter check [2..]
