{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrarySizedNatural)
import Test.QuickCheck.Gen (oneof, sized, Gen, frequency)
import Test.QuickCheck (quickCheckAll, chooseInt, generate, getSize)
import PQ (makeHeap, mergeHeaps, getMin, extractMin, checkHeapIntegrity, heapSize)
import Data.List (sort)
import Debug.Trace

minsHeapRepr heap = if heapSize heap == 0 then [] else map getMin (take n $ iterate extractMin heap)
    where n = heapSize heap


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

verifyOpsIntegrity (MakeHeap x) = (True, makeHeap x)
verifyOpsIntegrity (MergeHeaps sub1 sub2) = (r1 && r2 && checkHeapIntegrity (heapSize h) h, h)
    where
        (r1, h1) = verifyOpsIntegrity sub1
        (r2, h2) = verifyOpsIntegrity sub2
        h = mergeHeaps h1 h2

verifyOpsIntegrity (ExtractMin sub) = (r && checkHeapIntegrity (heapSize nheap) nheap, nheap)
    where 
        (r, h) = verifyOpsIntegrity sub
        nheap = extractMin h


prop_Ops_Elements opTree = listRepr == minsHeapRepr heapRepr && opsIntegrity
    where
        listRepr = emulateOps opTree
        heapRepr = executeOps opTree
        opsIntegrity = fst $ verifyOpsIntegrity opTree

return []
main = $quickCheckAll

--main = do
--    let tree = MergeHeaps (ExtractMin (ExtractMin (MergeHeaps (MakeHeap (-1)) (MergeHeaps (MakeHeap 0) (MakeHeap 1))))) (MakeHeap 0)
--    print $ emulateOps tree
--    print $ minsHeapRepr $ executeOps tree
--    print $ prop_Ops_Elements tree 
