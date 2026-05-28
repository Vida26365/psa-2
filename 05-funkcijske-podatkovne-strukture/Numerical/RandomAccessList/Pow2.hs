module RandomAccessList.Pow2
  ( Pow2 (..),
  )
where

class Pow2 t where
  singleton :: a -> t a
  linkTree :: t a -> t a -> t a
  sizeTree :: t a -> Int
  splitTree :: t a -> (t a, t a)
  lookupTree :: Int -> t a -> a
  updateTree :: Int -> a -> t a -> t a
