module Pow2.LeafTree
  ( LeafTree,
  )
where

import Pow2

data LeafTree a = Leaf a | Node Int (LeafTree a) (LeafTree a) deriving (Show)

instance Pow2 LeafTree where
  singleton = Leaf
  linkTree t1 t2 = Node (sizeTree t1 + sizeTree t2) t1 t2
  sizeTree (Leaf _) = 1
  sizeTree (Node w _ _) = w
  splitTree (Node _ t1 t2) = (t1, t2)
  lookupTree 0 (Leaf x) = x
  lookupTree i (Node w t1 t2)
    | i < w `div` 2 = lookupTree i t1
    | otherwise = lookupTree (i - w `div` 2) t2
  updateTree 0 y (Leaf _) = Leaf y
  updateTree i y (Node w t1 t2)
    | i < w `div` 2 = Node w (updateTree i y t1) t2
    | otherwise = Node w t1 (updateTree (i - w `div` 2) y t2)
