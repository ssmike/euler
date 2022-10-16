import qualified Data.List as L
import Control.Monad (forM_, when)

pithagoreans lim = do
    a::Int <- [1..lim]
    b::Int <- [1..a-1]
    let fst = 2*a*b
    let snd = a^2-b^2
    let third = a^2+b^2
    let perimeter = fst+snd+third
    [(fst*mul, snd*mul, third*mul) | gcd a b == 1, mul <- [1..div lim perimeter]]

perimeters lim = [a+b+c | (a, b, c) <- pithagoreans lim, a+b+c<=lim]

counts = map (\grp -> (length grp, head grp)) $ L.group $ L.sort $ perimeters 1000

main = do
    --print counts
    --print $ L.group $ L.sort $ pithagoreans 1000
    --print $ pithagoreans 120
    print $ maximum counts
    --forM_ (pithagoreans 120) $ \(a,b,c) ->
    --    print $ show a ++ "^2+" ++ show b ++ "^2-" ++ show c ++ "^2=" ++ show (a^2+b^2-c^2)
