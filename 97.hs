import Euler (digits, multiplyByMod, powerByMod)

num = 28433 * 2^7830457 + 1

modMul = multiplyByMod (10^10)

modPower = powerByMod (10^10)

main = do
    print $ (+1) $ 28433 `modMul` (modPower 2 7830457)
    --print $ reverse $ take 10 $ reverse $ digits num 10

