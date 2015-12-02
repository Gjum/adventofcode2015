import System.IO
import Data.List.Split

wrappingPaper line = minimum areas + 2 * sum areas
  where
    areas = [product [x, y] | (x,y) <- [(l,w),(w,h),(l,h)] ]
    l:w:h:_ = map read line :: [Int]

main = do
  withFile "2.in" ReadMode (\handle -> do
    contents <- hGetContents handle
    print $ sum (map wrappingPaper [splitOn "x" line | line <- lines contents])
    )

-- 1598415
