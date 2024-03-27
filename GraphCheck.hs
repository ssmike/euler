{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck.Arbitrary (Arbitrary(..), arbitrarySizedNatural)
import Test.QuickCheck.Gen (oneof, sized, Gen, frequency)
import Test.QuickCheck (quickCheckAll, chooseInt, generate, getSize, Positive(..))
import Data.List (sort)
import Data.Array (listArray, (!))
import Debug.Trace
import Graph


prop_Bamboo cells = dists == dijkstra graph (1::Int) (0::Int)
    where
        dists = zip [1..] $ scanl (+) 0 cells
        n = length cells
        graph = ListArrayGraph $ listArray (1, n+1) $ [ListArrayGraphNode {nodeid = i, incidenceList = [(i + 1, w)]} | (i, w) <- zip [1..n] cells ] ++ [ListArrayGraphNode { nodeid = n + 1, incidenceList = []} ]


return []
main = $quickCheckAll
