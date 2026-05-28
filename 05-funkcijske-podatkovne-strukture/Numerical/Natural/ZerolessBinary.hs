module Natural.ZerolessBinary
  ( ZLBinary,
    testZLBinary,
  )
where

import Data.List (intercalate)
import Natural

data Bit = One | Two

newtype ZLBinary = ZBin [Bit]

instance Show ZLBinary where
  show (ZBin ds) =
    concatMap showBit (reverse ds)
      ++ "₂"
      ++ " = "
      ++ intercalate " + " (zipWith (\i d -> showBit d ++ "⋅2" ++ powerOfTwo i) [0 ..] ds)
    where
      showBit One = "1"
      showBit Two = "2"

inc :: [Bit] -> [Bit]
inc [] = [One]
inc (One : ds) = Two : ds
inc (Two : ds) = One : inc ds

dec :: [Bit] -> [Bit]
dec [One] = []
dec (Two : ds) = One : ds
dec (One : ds) = Two : dec ds

addBits :: [Bit] -> [Bit] -> [Bit]
addBits [] ds2 = ds2
addBits ds1 [] = ds1
addBits (One : ds1) (One : ds2) = Two : addBits ds1 ds2
addBits (One : ds1) (Two : ds2) = One : inc (addBits ds1 ds2)
addBits (Two : ds1) (One : ds2) = One : inc (addBits ds1 ds2)
addBits (Two : ds1) (Two : ds2) = Two : inc (addBits ds1 ds2)

instance Natural ZLBinary where
  zero = ZBin []
  incr (ZBin ds) = ZBin (inc ds)
  decr (ZBin ds) = ZBin (dec ds)
  add (ZBin ds1) (ZBin ds2) = ZBin (addBits ds1 ds2)

testZLBinary :: [(String, ZLBinary)]
testZLBinary = testNatural