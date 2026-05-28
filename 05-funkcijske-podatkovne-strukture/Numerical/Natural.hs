module Natural
  ( Natural (..),
    fromInt,
    testNatural,
    powerOfTwo,
  )
where

class Natural n where
  zero :: n
  incr :: n -> n
  decr :: n -> n
  add :: n -> n -> n

fromInt :: Natural n => Integer -> n
fromInt 0 = zero
fromInt n = incr (fromInt (n - 1))

powerOfTwo :: Int -> String
powerOfTwo 0 = "⁰"
powerOfTwo n =
  map ("⁰¹²³⁴⁵⁶⁷⁸⁹" !!) (digits n)
  where
    digits 0 = []
    digits k = digits (k `div` 10) ++ [k `mod` 10]

testNatural :: Natural n => [(String, n)]
testNatural =
  let padTo7 s = replicate (7 - length s) ' ' ++ s
   in let testInc = map (\i -> (padTo7 (show i), fromInt i)) [0 .. 13]
          testDec = ("decr 14", decr (fromInt 14))
          testAdd = ("add 6 7", add (fromInt 6) (fromInt 7))
       in testInc ++ [testDec, testAdd]
