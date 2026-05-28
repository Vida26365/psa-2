import Natural.Peano
import Natural.Binary
import Natural.SkewBinary
import Natural.ZerolessBinary
import RandomAccessList
import RandomAccessList.List
import RandomAccessList.Sequence
import RandomAccessList.Skew
import RandomAccessList.Zeroless
import qualified Distribution.SPDX as Numerical

main :: IO ()
main =
  let printNumTest (s, n) = putStrLn $ "  " ++ s ++ " = " ++ show n
      printListTest (s, xs) = putStrLn $ "  " ++ s ++ " = " ++ show (map (`RandomAccessList.lookup` xs) [0 .. RandomAccessList.size xs - 1])
   in do
        putStrLn "Peanova naravna števila"
        mapM_ printNumTest Natural.Peano.testPeano
        putStrLn "Dvojiški zapis"
        mapM_ printNumTest Natural.Binary.testBinary
        putStrLn "Dvojiški zapis brez ničel"
        mapM_ printNumTest Natural.ZerolessBinary.testZLBinary
        putStrLn "Poševni dvojiški zapis"
        mapM_ printNumTest Natural.SkewBinary.testSkewBinary
        putStrLn "Verižni seznami"
        mapM_ printListTest RandomAccessList.List.testList
        putStrLn "Zaporedja"
        mapM_ printListTest RandomAccessList.Sequence.testSequence
        putStrLn "Zaporedja brez ničel"
        mapM_ printListTest RandomAccessList.Zeroless.testZerolessList
        putStrLn "Poševni seznami"
        mapM_ printListTest RandomAccessList.Skew.testSkewList
        putStrLn "Naraščajoči seznami"
        mapM_ printListTest RandomAccessList.Skew.testIncreasingList
