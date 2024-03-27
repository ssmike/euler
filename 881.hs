import Euler (primesGen)
import Control.Monad (forM_)

polyAdd :: Num a => [a] -> [a] -> [a]
polyAdd [] x = x
polyAdd x [] = x
polyAdd allx@(x:xs) ally@(y:ys) = (x+y) : polyAdd xs ys

polyMultiply :: Num a => [a] -> [a] -> [a]
polyMultiply [] _ = []
polyMultiply _ [] = []
polyMultiply allx@(x:xs) ys =  map (*x) ys `polyAdd` (0: polyMultiply xs ys)
        

hyperCubeWidth dims = maximum $ foldl1 polyMultiply $ map (\x -> replicate (x+1) 1) dims

allPossibleDims :: [[Int]]
allPossibleDims = [1..] >>= allPossibleDimsOf
    where
        allPossibleDimsOf :: Int -> [[Int]]
        allPossibleDimsOf 0 = [[]]
        allPossibleDimsOf n = do
            first <- reverse [1..n]
            map (first:) <$> allPossibleDimsOf $ n - first

makeNumber dims = product $ zipWith (^) primesGen dims

main = do
    print $ hyperCubeWidth [1, 2]
    let goodDims = map makeNumber $ filter ((>=10^4) . hyperCubeWidth) allPossibleDims
    forM_ (scanl1 min goodDims) print 
