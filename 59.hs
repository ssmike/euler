import Data.List.Split (splitOn)
import Data.List (groupBy, sort)
import Data.Bits (xor)

allChrs = enumFromTo 'a' 'z' ++ enumFromTo 'A' 'Z'
allNums = map fromEnum allChrs
chooseDigits vars = filter cond $ enumFromTo 'a' 'z'
    where
        xrchr x y = toEnum $ fromEnum x `xor` fromEnum y
        cond var = and [(var `xrchr` base) `elem` allChrs | base <- vars]

main = do
    file <- readFile "/home/ssmike/Downloads/0059_cipher.txt"
    let nums = zip (map (`mod`3) [0..]) $ map (read :: String -> Int) $ splitOn "," file
    let alls = map (map snd) $ groupBy (\a b -> fst a == fst b) $ sort nums
    print $ map chooseDigits alls
