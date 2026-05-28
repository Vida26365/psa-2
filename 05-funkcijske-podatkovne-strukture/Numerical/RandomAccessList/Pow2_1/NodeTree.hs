module RandomAccessList.Pow2_1.NodeTree
  ( NodeTree,
  )
where

import RandomAccessList.Pow2_1

data NodeTree a = Leaf2 | Node2 Int a (NodeTree a) (NodeTree a) deriving (Show)

instance Pow2_1 NodeTree where
  empty = Leaf2
  linkTree x t1 t2 = Node2 (sizeTree t1 + sizeTree t2 + 1) x t1 t2
  sizeTree :: NodeTree a -> Int
  sizeTree Leaf2 = 0
  sizeTree (Node2 w _ _ _) = w
  splitTree (Node2 _ x t1 t2) = (x, t1, t2)
  lookupTree 0 (Node2 _ x _ _) = x
  lookupTree i (Node2 w _ t1 t2)
    | i <= w `div` 2 = lookupTree (i - 1) t1
    | otherwise = lookupTree (i - (w `div` 2) - 1) t2
  updateTree 0 y (Node2 w _ t1 t2) = Node2 w y t1 t2
  updateTree i y (Node2 w x t1 t2)
    | i <= w `div` 2 = Node2 w x (updateTree (i - 1) y t1) t2
    | otherwise = Node2 w x t1 (updateTree (i - (w `div` 2) - 1) y t2)
