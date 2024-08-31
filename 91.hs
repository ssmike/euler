sqlen (x, y) = x * x + y * y
scal (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

main = do
    let fldsz = 50
    let sm1 = fldsz * fldsz
    let fld = [(x, y) | x <- [0..fldsz], y <- [0..fldsz], x > 0 || y > 0]
    let sm2 = [(p1, p2) | p1 <- fld, p2 <- fld, sqlen p1 == scal p1 p2 && p1 /= p2]
    print $ take 5 sm2
    print $ sm1 + length sm2
