import System.IO
import Data.List.Split
import Data.Map (Map, (!), insert)
import qualified Data.Map as Map
import Data.Int
import Data.Bits
import Debug.Trace

main = do
  ls <- lines <$> readFile "07.in"
  --ls <- take 10 <$> lines <$> readFile "07.in"
  --let instr = Map.fromList [("a", ["bb"]),
  --                          ("bb", ["1", "LSHIFT", "ccc"]),
  --                          ("ccc", ["127", "AND", "3"])]

  --let instr = Map.fromList $ map indexify ["123 -> x",
  --                                        "456 -> y",
  --                                        "x AND y -> d",
  --                                        "x OR y -> e",
  --                                        "x LSHIFT 2 -> f",
  --                                        "y RSHIFT 2 -> g",
  --                                        "NOT x -> h",
  --                                        "NOT y -> i"]
  --print $ [(x, eval instr (x:[])) | x <- "defghixy"]

  let instr = Map.fromList $ map indexify ls
  print $ fst $ eval instr "a" -- 16076

  let instr2 = insert "b" ["16076"] $ Map.fromList $ map indexify ls
  print $ fst $ eval instr2 "a" -- 2797

indexify line = (out, reverse r)
  where out:"->":r = reverse $ splitOn " " line

-- cable name or number to number, may lookup cable name
eval :: Map [Char] [[Char]] -> [Char] -> (Int16, Map [Char] [[Char]])
eval m str = case (reads str) :: [(Int16, String)] of
  [(num, "")] -> (num, m)
  _ -> (num, insert str [show num] newm)
        where (num, newm) = parse m (m ! str)
  --_ -> trace ("touching "++str++" which is "++(show (m ! str))++" and becomes "++(show $ parse m (m ! str))) (parse m (m ! str))

-- operation to number
parse :: Map [Char] [[Char]] -> [[Char]] -> (Int16, Map [Char] [[Char]])
parse m (input:[]) = eval m input
parse m ("NOT":input:[]) = (complement $ num, newm)
  where (num, newm) = eval m input
parse m (inA:op:inB:[]) = (doOp op a b, mB)
  where (a, mA) = eval m inA
        (b, mB) = eval mA inB

doOp "OR" a b = a .|. b
doOp "AND" a b = a .&. b
doOp "LSHIFT" a b = a `shiftL` (fromIntegral $ b)
doOp "RSHIFT" a b = a `shiftR` (fromIntegral $ b)
