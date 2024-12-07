import Data.List.Split (splitOn)
import Data.List (groupBy, sort)
import Data.Bits (xor)

xrchr :: Char -> Char -> Char
xrchr x y = toEnum $ fromEnum x `xor` fromEnum y

allChrs = enumFromTo 'a' 'z' ++ enumFromTo 'A' 'Z' ++ " ;:,.'?-!()" ++ enumFromTo '0' '9'
allNums = map fromEnum allChrs

chooseDigits vars = filter cond $ map fromEnum $ enumFromTo 'a' 'z'
    where
        cond var = and [(var `xor` toEnum base) `elem` allNums | base <- vars]

main = do
    -- print $ xrchr 'A' 'k'
    file <- readFile "/home/ssmike/Downloads/0059_cipher.txt"
    let nums = zip (map (`mod`3) [0..]) $ map (read :: String -> Int) $ splitOn "," file
    print $ sort nums
    -- print $ map ((toEnum :: Int -> Char) . snd) nums
    let alls =  map (map snd) $ groupBy (\a b -> fst a == fst b) $ sort nums
    -- print alls
    print $ map chooseDigits alls
