import Euler (digits)

canon (a, b) = let g = gcd a b in (a `div` g, b `div` g)
addOne (a, b) = (a + b, b)
inv (a, b) = (b, a)

seed = (3, 2)
next = canon . addOne . inv . addOne

expansions = iterate next seed

digLen 0 = 0
digLen x = 1 + digLen (x `div` 10)

main = do
    print $ map digLen [1..10]
    print $ length $ filter (\(a, b) -> digLen a > digLen b) $ take 1000 expansions
