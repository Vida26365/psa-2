module Natural.SkewBinary
  ( SkewBinary,
    testSkewBinary,
  )
where

import Data.List (intercalate)
import Natural

newtype SkewBinary = SBin [Int]

instance Show SkewBinary where
  show (SBin ws) = intercalate " + " (map show ws)

inc :: [Int] -> [Int]
inc (w1 : w2 : ws) | w1 == w2 = (1 + w1 + w2) : ws
inc ws = 1 : ws

dec :: [Int] -> [Int]
dec (1 : ws) = ws
dec (w : ws) = (w `div` 2) : (w `div` 2) : ws

addBits :: [Int] -> [Int] -> [Int]
addBits [] ds2 = ds2
addBits ds1 ds2 = inc (addBits (dec ds1) ds2)

instance Natural SkewBinary where
  zero = SBin []
  incr (SBin ds) = SBin (inc ds)
  decr (SBin ds) = SBin (dec ds)
  add (SBin ds1) (SBin ds2) = SBin (addBits ds1 ds2)

testSkewBinary :: [(String, SkewBinary)]
testSkewBinary = testNatural