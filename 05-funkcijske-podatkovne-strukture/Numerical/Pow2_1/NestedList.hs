module Pow2_1.NestedList
  ( NestedList,
  )
where

import Pow2_1

data NestedList a = Nil | Cons a (NestedList (a, a))

zipInc :: NestedList a -> NestedList a -> NestedList (a, a)
zipInc Nil Nil = Nil
zipInc (Cons x xs) (Cons y ys) = Cons (x, y) (zipInc xs ys)

unzipInc :: NestedList (a, a) -> (NestedList a, NestedList a)
unzipInc Nil = (Nil, Nil)
unzipInc (Cons (x, y) t) =
  let (t1, t2) = unzipInc t
   in (Cons x t1, Cons y t2)

instance Pow2_1 NestedList where
  empty = Nil
  linkTree x t1 t2 = Cons x (zipInc t1 t2)
  sizeTree Nil = 0
  sizeTree (Cons _ t) = 1 + 2 * sizeTree t
  splitTree (Cons x t) =
    let (t1, t2) = unzipInc t
     in (x, t1, t2)
  lookupTree 0 (Cons x _) = x
  lookupTree i (Cons _ ts) =
    let (t1, t2) = unzipInc ts
        w = sizeTree t1
     in if i <= w then lookupTree (i - 1) t1 else lookupTree (i - w - 1) t2
  updateTree 0 y (Cons _ t) = Cons y t
  updateTree i y (Cons x t) =
    let (t1, t2) = unzipInc t
        w = sizeTree t1
     in if i <= w then linkTree x (updateTree (i - 1) y t1) t2 else linkTree x t1 (updateTree (i - w - 1) y t2)
