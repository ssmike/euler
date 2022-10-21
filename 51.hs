import Euler (digits, millerRabinTest)
import qualified System.Random as R
import Data.List
import Control.Monad
import Text.Printf

subsets :: Int -> [[Bool]]
subsets 0 = [[]]
subsets n = do
    other <- subsets $ n - 1
    map (:other) [True, False]

ones = ones_ . reverse
    where
        ones_ [] = 0
        ones_ (True:xs) = 1 + 10 * ones_ xs
        ones_ (False:xs) = 10 * ones_ xs

same (x:xs) = all (==x) xs

primeGroups :: (Integer -> Bool) -> Integer -> [(Int, Integer, [Integer])]
primeGroups checkPrime lim = do
    n <- [1..lim]
    guard $ checkPrime n
    let ndigits = n `digits` 10
    let len = length ndigits
    toChange <- subsets len
    guard $ not (and toChange) && or toChange
    let digitsToChange = map snd $ filter fst $ zip toChange ndigits
    guard $ same digitsToChange
    let commonDigit = head digitsToChange
    let group = map snd $ do
            digit <- [0..9-commonDigit]
            let applyTemplate = ones toChange
            let value = n + digit * applyTemplate
            guard $ checkPrime value
            return (n, value)
    guard $ group /= []
    return (- (length group), minimum group, group)


genNumbers = R.randoms . R.mkStdGen <$> R.randomIO

main = do
    --print $ digits 765 10
    --print $ makeNumber [7, 6, 5]
    primeCandidates <- take 40 `fmap` genNumbers
    --print $ zip [1..] $ map (checkPrime primeCandidates) [1..30]
    --print $ primeGroups (millerRabinTest primeCandidates) 20
    forM_ (iterate (*10) 10) $ \lim -> do
        printf "searching up to %d\n" lim 
        let groups = primeGroups (millerRabinTest primeCandidates) lim
        when (groups /= []) $ print $ minimum groups

