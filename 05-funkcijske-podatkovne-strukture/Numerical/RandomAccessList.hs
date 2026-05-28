module RandomAccessList
  ( RandomAccessList (..),
    fromList,
    testRandomAccessList,
  )
where

import Prelude hiding (head, lookup, tail)

class RandomAccessList f where
  nil :: f a
  cons :: a -> f a -> f a
  head :: f a -> a
  tail :: f a -> f a
  append :: f a -> f a -> f a
  lookup :: Int -> f a -> a
  update :: Int -> a -> f a -> f a
  size :: f a -> Int

fromList :: RandomAccessList f => [a] -> f a
fromList [] = nil
fromList (x : xs) = cons x (fromList xs)

testRandomAccessList :: RandomAccessList f => [(String, f Int)]
testRandomAccessList =
  let xs = fromList [0 .. 6]
      ys = fromList [7 .. 13]
      testAppend = ("append", append xs ys)
      testUpdate = ("update", update 6 6767 (append xs ys))
   in [testAppend, testUpdate]