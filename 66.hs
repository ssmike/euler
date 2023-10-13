import Control.Monad (guard, forM_, forM, when)
import qualified Data.Set as Set
import qualified Data.Array.ST as S
import qualified Control.Monad.ST as ST
import Data.Array
import Euler (divisorsUpTo, multiples)


multiply mul (a, b) (c, d) = (a*c + mul*b*d, b*c + a*d)
conjugate (a, b) = (a, -b)

norm :: Integer -> (Integer, Integer) -> Integer
norm mul c  = fst $ multiply mul c (conjugate c)

solutions :: Integer -> [Integer]
solutions d = do
    --i <- [1..]
    j <- [1..]
    let isq = 1 + d * j^2
    guard $ isSquare isq
    --guard $ i^2 - d * j^2 == 1
    return isq


isSquare x = x == floor (sqrt $ fromIntegral x) ^2

soultionsUpTo lim = do
    d <- [1..lim]
    guard $ not $ isSquare d
    let sol = solutions d
    guard $ sol /= []
    return (head sol, d)

divisorsChain = map divisorsUpTo $ iterate (*2) 20000

multiples2 :: Integer -> [(Integer, Int)]
multiples2 1 = []
multiples2 n = divisors n
    where
        divisorsArr = head $ dropWhile ((<n) . snd . bounds) divisorsChain

        divBy x n
            | n `mod` x /= 0 = (0, n)
            | otherwise = let (pow, val) = divBy x (n `div` x)
                          in (pow + 1, val)

        divisors 1 = []
        divisors x = (firstDiv, power): divisors remainder
            where 
                firstDiv = let val = divisorsArr ! x in min val x 
                (power, remainder) = divBy firstDiv x


genSquares :: Integer -> [Integer]
genSquares 0 = []
genSquares x = do
    let muls = multiples x
    powers <- forM muls  $ \(mul, pow) -> take ((pow `div` 2)+1) $ iterate (*mul^2) 1
    return $ product powers


genDs = do
    x <- [1..]
    let sum = x^2 - 1
    y2 <- genSquares sum
    let d = sum `div` y2
    guard $ d > 0
    guard $ d <= 1000
    return (x, d) 


estimate = guardIf $ scanl addPair seed genDs
    where
        guardIf = takeWhile (\(x, s) -> Set.size s > 0)
        allLens = foldl (flip Set.insert) Set.empty (filter (not.isSquare) [1..1000])
        addPair (cur, seen) (x, d) = (x, Set.delete d seen)
        seed = (0, allLens)
        

main = do
    forM_ estimate print
    --mapM_ (\x -> print (x, multiples2 x == multiples x)) [1..200000]
    --print $ maximum $ soultionsUpTo 7
    --forM_ (zip (scanl1 max $ soultionsUpTo 1000) [1..]) print
    --print $ map (head.solution) [5..7]
