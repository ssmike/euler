import Control.Monad (forM_, guard)

binom :: Integer -> Integer -> Integer
binom n k = (product [n - i| i <- [0..k-1]]) `div` product [1..k]

countRects w h = binom (w + 1) 2 * binom (h + 1) 2

allDims = do
    perimeter <- [1..]
    width <- [1..perimeter - 1]
    return (width, perimeter - width)

allNums = map (\(w, h)-> (w * h, countRects w h)) allDims 

limit = 2*10^6
--limit = 18

allBinoms :: [(Integer, Integer)]
allBinoms = takeWhile ((<=2*limit) . snd) $ [(n, binom (n+1) 2) | n <- [1..]]

findDivs = do
    (fs, fsCnt) <- allBinoms
    (sc, scCnt) <- allBinoms
    --guard $ fsCnt*scCnt == limit
    return (abs $ limit - fsCnt*scCnt, fs, sc, fs*sc)

main = do
    print $ countRects 3 2
    --print allBinoms
    forM_ (scanl1 min findDivs) print

