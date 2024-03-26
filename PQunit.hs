import PQ (makeHeap, mergeHeaps, getMin, extractMin, checkHeapIntegrity, heapSize)
import Test.HUnit
import Control.Monad (forM_)
import Data.List (sort)

assertIntegrity size heap = assertBool ("integrity test doesn't hold for size " ++ show size ++ " " ++ show heap) $ checkHeapIntegrity size heap

trivial = TestCase $ assertIntegrity 1 $ makeHeap (2::Int)
constructionTest n = TestCase $ assertIntegrity (n*2) $ foldl1 mergeHeaps (map makeHeap $ [1..n]++[1..n])

extractionTest n = TestCase $ do
    let values = [1..n] ++ [1..n]
    let bigHeap = foldl1 mergeHeaps (map makeHeap values)
    assertIntegrity (length values) bigHeap
    --assertBool (show bigHeap) False
    assertIntegrity ((length values) - 1) (extractMin bigHeap)
    let smallerHeaps = take (length values) $ iterate extractMin bigHeap
    let triplets = zip3 (sort values) (reverse [1..length values]) smallerHeaps
    forM_ triplets $ \(minValue, size, heap) -> do
        assertEqual "size holds " size (heapSize heap)
        assertEqual "min value holds" minValue (getMin heap)
        assertIntegrity size heap

pqtests = TestList [
    TestLabel "trivial heap" trivial,
    TestLabel "small heap construction" $ constructionTest 3,
    TestLabel "big heap construction" $ constructionTest 100,
    TestLabel "small heap extraction" $ extractionTest 3,
    TestLabel "big heap extraction" $ extractionTest 100
    ]

main = runTestTT pqtests
