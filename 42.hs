import qualified Data.Text.Lazy as L
import Data.List.Split (splitWhen)
import Data.List (find)
import Data.Char (ord)

unquote = filter (/='\"')

sumChars word = sum $ map charVal word
    where
        charVal c = ord c - ord 'A' + 1

triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]
isTriangle n = bound == n
    where 
        bound = head nums
        nums = dropWhile (<n) triangles

main = do
    lines <- readFile "/home/ssmike/Downloads/p042_words.txt"
    let words = map unquote $ splitWhen (==',') lines
    let triangles = filter (isTriangle.sumChars) words
    print triangles
    print $ length triangles
