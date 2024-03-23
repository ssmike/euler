import Data.List (sort, groupBy, sortBy)
import Data.Char (digitToInt)
import Control.Monad (forM_, guard)

explore :: [[Int]] -> [[Int]]
explore [] = [[]]

explore triplets = do
    (weight, digit, newTriplets) <- sortBy (\(a,_,_) (b,_,_) -> compare b a) candidateList
    guard $ weight > 0
    result <- explore newTriplets
    return $ digit:result
    where
        candidateList = do
            digit <- [0..9]
            let cutOff lst = if digit == head lst then tail lst else lst
            let cost triplets = sum $ map length triplets
            let newTriplets = filter (/=[]) $ map cutOff triplets
            return (cost triplets - cost newTriplets, digit, newTriplets)
    
sequences triplets = map (\x -> (length x, x)) $ explore triplets    

replay [] triplets = [triplets]
replay (digit:seq) triplets = triplets: replay seq nextTriplets
    where
        cutOff lst = if digit == head lst then tail lst else lst
        nextTriplets = filter (/=[]) $ map cutOff triplets
        


main = do
    chrs <- lines `fmap` readFile "/home/ssmike/Downloads/0079_keylog.txt"
    let triplets = map (map digitToInt) chrs
    forM_ (scanl1 min $ sequences triplets) $ print . show
    --forM_ (replay ([7,3,1,6,2,8,9,0]::[Int]) triplets) $ print . show

