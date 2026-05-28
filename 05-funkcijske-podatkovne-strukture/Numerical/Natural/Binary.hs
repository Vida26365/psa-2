module Natural.Binary
  ( Binary,
    testBinary,
  )
where

import Data.List (intercalate)
import Natural

data Bit = O | I

newtype Binary = Bin [Bit]

instance Show Binary where
  show (Bin ds) =
    concatMap showBit (reverse ds)
      ++ "₂"
      ++ " = "
      ++ intercalate " + " (zipWith (\i d -> showBit d ++ "⋅2" ++ powerOfTwo i) [0 ..] ds)
    where
      showBit I = "1"
      showBit O = "0"

inc :: [Bit] -> [Bit]
inc [] = [I]
inc (O : ds) = I : ds
inc (I : ds) = O : inc ds

dec :: [Bit] -> [Bit]
dec [I] = []
dec (I : ds) = O : ds
dec (O : ds) = I : dec ds

addBits :: [Bit] -> [Bit] -> [Bit]
addBits [] ds2 = ds2
addBits ds1 [] = ds1
addBits (O : ds1) (d : ds2) = d : addBits ds1 ds2
addBits (d : ds1) (O : ds2) = d : addBits ds1 ds2
addBits (I : ds1) (I : ds2) = O : inc (addBits ds1 ds2)

instance Natural Binary where
  zero = Bin []
  incr (Bin ds) = Bin (inc ds)
  decr (Bin ds) = Bin (dec ds)
  add (Bin ds1) (Bin ds2) = Bin (addBits ds1 ds2)

testBinary :: [(String, Binary)]
testBinary = testNatural