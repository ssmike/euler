import Euler (pithagoreans, uniqueList)
import Control.Monad (forM_)
import Data.List (group, sort)

limit = 15*10^5

canonize (a,b,c) = if a > b then (b,a,c) else (a,b,c)

main = do
    let uniqTriplets = uniqueList $ map canonize $ pithagoreans limit
    let perimeters = map (\(a,b,c) -> a+b+c) uniqTriplets 
    let grouped = filter ((==1) . length) $ group $ sort perimeters
    --let grouped = group $ sort triplets
    --forM_ (take 20 $ grouped) print
    --print "perimeter"
    --forM_ grouped $ print . head
    print $ length grouped
    ----print $ length triplets
