module Natural.Peano
  ( Peano,
    testPeano,
  )
where

import Natural

data Peano = Zero | Succ Peano

instance Show Peano where
  show Zero = "0"
  show (Succ n) = "1+" ++ show n

predPeano :: Peano -> Peano
predPeano (Succ n) = n

addPeano :: Peano -> Peano -> Peano
addPeano Zero m = m
addPeano (Succ n) m = Succ (addPeano n m)

instance Natural Peano where
  zero = Zero
  incr = Succ
  decr = predPeano
  add = addPeano

testPeano :: [(String, Peano)]
testPeano = testNatural