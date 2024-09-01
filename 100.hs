import Euler (integralSqrt)
import Control.Monad (forM_)

checkNumber x = m * (m + 1) == x
    where
        m = integralSqrt x
    
chain = map (\x -> x * (x - 1)) [10^12+1..]

filteredChain = filter (checkNumber . (`div`2))

main = do
    print $ checkNumber $ 21 * 20
    print $ take 10 $ filter even chain
    let mults = filter (checkNumber . (`div` 2)) $ filter even chain
    let blues = map ((+1) . integralSqrt . (`div`2)) mults
    forM_ blues print
    --print $ map ((+1) . integralSqrt . (`div`2)) mults
    --print $ map ((+1) . integralSqrt) $ take 20 filteredChain
    
