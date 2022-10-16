import Control.Monad (forM_)
import Euler
import Data.Array
import Data.Map.Internal.Debug (balanced)

digitFacs = map factorial [0..9]


sumDigitFacs 0 = 0
sumDigitFacs n = factorial digit + sumDigitFacs (n `div` 10)
    where digit = n `mod` 10

calcLim = takeWhile (<10) rationals
    where
        maxDigit = 9::Int
        fac = digitFacs !! maxDigit
        fractions = drop 1 $ iterate (\(digits, facs) -> (digits * 10 + maxDigit, facs + fac)) (0, 0)
        rationals = map (\(a, b) -> (fromIntegral a :: Rational) / fromIntegral b) fractions



balances = map balancesFor [0..]
    where
        balancesFor n = map (\i -> 10^n*i - factorial i) [0..9]

maxDigits = length $ takeWhile (<=0) minSumBalances
    where
        minBalances = map minimum balances
        minNonZero = map (minimum . tail) balances
        minSumBalances = zipWith (+) minNonZero (scanl (+) 0 minBalances)

balancesA = array ((0, 0), (maxDigits, 9)) [((num, digit), balances !! num !! digit) | num <- [0..maxDigits], digit <- [0..9]]

perfectNumbers = [2..maxDigits] >>= sumPerfectNumbersFor
    where
        sumPerfectNumbersFor len = explore 0 len 0 0
        explore num lim balance numAcc =
            if num < lim then do
                digit <- [(if num + 1 == lim then 1 else 0)..9]
                explore (num+1) lim (balance + balancesA ! (num, digit)) (numAcc + digit * 10^num)
            else [numAcc | balance == 0]

main = do
    --print digitFacs
    --forM_ (take 15 balances) print
    --print maxDigits
    --print balancesA
    --print $ balancesA ! (5, 7)
    --print $ 7 * 10^5 - factorial 7
    print perfectNumbers
    print $ sum perfectNumbers
