module Pow2_1.NodeTree
  ( NodeTree,
  )
where

import Pow2_1

data NodeTree a = Leaf | Node Int a (NodeTree a) (NodeTree a) deriving (Show)

instance Pow2_1 NodeTree where
  empty = Leaf
  linkTree x t1 t2 = Node (sizeTree t1 + sizeTree t2 + 1) x t1 t2
  sizeTree :: NodeTree a -> Int
  sizeTree Leaf = 0
  sizeTree (Node w _ _ _) = w
  splitTree (Node _ x t1 t2) = (x, t1, t2)
  lookupTree 0 (Node _ x _ _) = x
  lookupTree i (Node w _ t1 t2)
    | i <= w `div` 2 = lookupTree (i - 1) t1
    | otherwise = lookupTree (i - (w `div` 2) - 1) t2
  updateTree 0 y (Node w _ t1 t2) = Node w y t1 t2
  updateTree i y (Node w x t1 t2)
    | i <= w `div` 2 = Node w x (updateTree (i - 1) y t1) t2
    | otherwise = Node w x t1 (updateTree (i - (w `div` 2) - 1) y t2)
