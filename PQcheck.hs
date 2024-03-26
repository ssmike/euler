{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrarySizedNatural)
import Test.QuickCheck.Gen (oneof, sized, Gen, frequency)
import Test.QuickCheck (quickCheckAll, chooseInt, generate, getSize)
import PQ (makeHeap, mergeHeaps, getMin, extractMin, emptyHeap, checkHeapIntegrity, heapSize)
import Data.List (sort)
import Debug.Trace

data MergeTree = Leaf Int | MergeTree MergeTree MergeTree
    deriving Show

getMinFromTree (Leaf x) = x
getMinFromTree (MergeTree sub1 sub2) = min (getMinFromTree sub1) (getMinFromTree sub2)

makeHeapFromTree (Leaf x) = makeHeap x
makeHeapFromTree (MergeTree sub1 sub2) = mergeHeaps (makeHeapFromTree sub1) (makeHeapFromTree sub2)

buildListRepresentation (Leaf x) = [x]
buildListRepresentation (MergeTree sub1 sub2) = sort $ buildListRepresentation sub1 ++ buildListRepresentation sub2

arbitraryTreeOfSize :: Int -> Gen MergeTree
arbitraryTreeOfSize 0 = arbitraryTreeOfSize 1
arbitraryTreeOfSize 1 = Leaf `fmap` arbitrary
arbitraryTreeOfSize size = do
    sub <- chooseInt (1, size-1)
    sub1 <- arbitraryTreeOfSize sub
    sub2 <- arbitraryTreeOfSize $ size - sub
    return $ MergeTree sub1 sub2

instance Arbitrary MergeTree where
    arbitrary = sized arbitraryTreeOfSize


prop_Min :: MergeTree -> Bool
prop_Min tree = getMinFromTree tree == getMin (makeHeapFromTree tree) 

minsHeapRepr heap = if heapSize heap == 0 then [] else map getMin (take n $ iterate extractMin heap)
    where n = heapSize heap


prop_Seq_Min tree = listRepr == minsHeapRepr heapRepr
    where
        listRepr = buildListRepresentation tree
        heapRepr = makeHeapFromTree tree
        n = length listRepr


data OpTree = MakeHeap Int | ExtractMin OpTree | MergeHeaps OpTree OpTree
    deriving Show

arbitraryOpTree :: Int -> Int -> Gen OpTree
arbitraryOpTree 0 0 = arbitraryOpTree 1 0
arbitraryOpTree 1 0 = MakeHeap `fmap` arbitrary
arbitraryOpTree size extractions = frequency [(max 0 (size - 1), randomCut), (extractions, ExtractMin <$> arbitraryOpTree (size+1) (extractions - 1))]
    where 
        randomCut = do
            sub <- chooseInt (1, size-1)
            subExtractions <- chooseInt (0, extractions)
            sub1 <- arbitraryOpTree sub subExtractions
            sub2 <- arbitraryOpTree (size - sub) (extractions - subExtractions)
            return $ MergeHeaps sub1 sub2


instance Arbitrary OpTree where
    arbitrary = do
        size <- getSize
        extracts <- chooseInt (1, size)
        arbitraryOpTree size extracts

    shrink (MakeHeap _) = []
    shrink (ExtractMin x) = [x]
    shrink (MergeHeaps a b) = [a, b]


emulateOps (MakeHeap x) = [x]
emulateOps (MergeHeaps sub1 sub2) = sort $ emulateOps sub1 ++ emulateOps sub2
emulateOps (ExtractMin sub) = tail $ emulateOps sub


executeOps (MakeHeap x) = makeHeap x
executeOps (MergeHeaps sub1 sub2) = mergeHeaps (executeOps sub1) (executeOps sub2)
executeOps (ExtractMin sub) = extractMin $ executeOps sub

prop_Ops_Elements opTree = reverse listRepr == minsHeapRepr heapRepr
    where
        listRepr = emulateOps opTree
        heapRepr = executeOps opTree
        n = length listRepr

return []
main = $quickCheckAll

--main = do
--    tree :: OpTree <- generate $ arbitraryOpTree 0 3
--    print tree
    --print $ getMinFromTree tree
    --print $ getMin $ makeHeapFromTree tree
