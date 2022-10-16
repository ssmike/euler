import qualified Data.List as L
import Control.Monad (forM_, when)
import Euler (pithagoreans)


perimeters lim = [a+b+c | (a, b, c) <- pithagoreans lim, a+b+c<=lim]

counts = map (\grp -> (length grp, head grp)) $ L.group $ L.sort $ perimeters 1000

main = do
    --print counts
    --print $ L.group $ L.sort $ pithagoreans 1000
    --print $ pithagoreans 120
    print $ maximum counts
    --forM_ (pithagoreans 120) $ \(a,b,c) ->
    --    print $ show a ++ "^2+" ++ show b ++ "^2-" ++ show c ++ "^2=" ++ show (a^2+b^2-c^2)
