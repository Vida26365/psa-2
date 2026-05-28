module RandomAccessList.Pow2_1
  ( Pow2_1 (..),
  )
where

class Pow2_1 t where
  empty :: t a
  linkTree :: a -> t a -> t a -> t a
  sizeTree :: t a -> Int
  splitTree :: t a -> (a, t a, t a)
  lookupTree :: Int -> t a -> a
  updateTree :: Int -> a -> t a -> t a
