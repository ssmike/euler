import Euler (integralSqrt, intersectSorted)
import Control.Monad (forM_)

checkNumber x = m * (m + 1) == x
    where
        m = integralSqrt x
    
chainFrom x = map (\x -> x * (x - 1)) [x..]

bound = 10^12

chain = chainFrom $ bound+1
chainHalf = map (*2) $ chainFrom $ integralSqrt (bound^2`div`2)

filteredChain = filter (checkNumber . (`div`2))

main = do
    --print $ checkNumber $ 21 * 20
    --print $ take 10 $ filter even chain
    --let mults = filter (checkNumber . (`div` 2)) $ filter even chain
    --let blues = map ((+1) . integralSqrt . (`div`2)) mults
    --forM_ blues print
    let nums = intersectSorted chain chainHalf
    forM_ nums $ \x -> print (1 + integralSqrt x, 1 + integralSqrt (x`div`2))
    
