import Euler
import qualified Data.List as L

checkPermute :: [Int] -> [Int]
checkPermute list = indices
    where
        toNum = foldl (\acc r -> acc * 10 + r) 0
        slice start len = toNum $ take len $ drop start list

        len = length list
        indices = do
            i <- [1..len]
            j <- [1..len-i]
            if i + j < len then
                let
                    mul1 = slice 0 i
                    mul2 = slice i j
                    res = slice (i + j) len
                in [res | mul1 * mul2 == res]
            else []

main = let checked = allPermutes [1..9] >>= checkPermute in do
    print checked
    print $ sum $ uniqueList checked

