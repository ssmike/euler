import Euler (digits, multiplyByMod, powerByMod, remainderBy, RemaindersRing)

rem10 :: Integer -> RemaindersRing Integer
rem10 = remainderBy (10^10)

main = do
    print $ (+1) $ 28433 * (rem10 2 ^ 7830457)

