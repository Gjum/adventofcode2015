import System.IO
import Data.List.Split
import qualified Data.Sequence as S

main = do
  actions <- map parse <$> lines <$> readFile "06.in"
  print $ sum $ fmap sum $ foldl (switchLights switchLamp1) initialLights actions -- 543903
  print $ sum $ fmap sum $ foldl (switchLights switchLamp2) initialLights actions -- 14687245

initialLights = S.replicate 1000 $ S.replicate 1000 0

parse :: [Char] -> ([Char], (Int,Int), (Int,Int))
parse line = (action, (fx,fy), (tx,ty))
  where
    tos:_:froms:action:_ = reverse $ splitOn " " line
    tx:ty:_ = map read $ splitOn "," tos
    fx:fy:_ = map read $ splitOn "," froms

switchLights :: ([Char] -> Int -> Int) -> S.Seq (S.Seq Int) -> ([Char], (Int,Int), (Int,Int)) -> S.Seq (S.Seq Int)
switchLights switchLamp board (action, (fx,fy), (tx,ty)) = newboard
  where
    newboard = S.mapWithIndex rerow board
    rerow y = fromTo fy ty y $ S.mapWithIndex recol
    recol x = fromTo fx tx x $ switchLamp action

fromTo :: Int -> Int -> Int -> (a -> a) -> (a -> a)
fromTo from to index fun =
  if from <= index && index <= to
    then fun
    else id

switchLamp1, switchLamp2 :: [Char] -> Int -> Int

switchLamp1 "on" _ = 1
switchLamp1 "off" _ = 0
switchLamp1 "toggle" l = 1-l

switchLamp2 "on" l = l+1
switchLamp2 "off" l = if l > 0 then l-1 else 0
switchLamp2 "toggle" l = l+2
