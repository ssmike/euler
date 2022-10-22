import Euler (makeNumber, digits)

reverseNumber = makeNumber . reverse . (`digits`10)

lychrelSeries = drop 1 . iterate (\x -> x + reverseNumber x)

lychrel :: Integer -> Bool
lychrel x = null $ dropWhile (not.isPoly) xs
    where
        xs = take 60 $ lychrelSeries x
        isPoly x = reverseNumber x == x 

main = print $ length $ filter lychrel [1..10000]
