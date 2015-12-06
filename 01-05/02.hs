import System.IO
import Data.List.Split
import Data.List

wrappingPaper line = wrap + slack
  where
    wrap = 2 * sum areas
    slack = minimum areas
    areas = [ x*y | (x,y) <- [(w,h),(h,l),(l,w)] ]
    w:h:l:_ = line

ribbonLength line = wrap + bow
  where
    wrap = w+w + h+h
    bow = w*h*l
    w:h:l:_ = sort line

presentSizes contents = [map read (splitOn "x" line) :: [Int] | line <- contents]

main = do
  contents <- lines <$> readFile "02.in"
  print $ sum (map wrappingPaper (presentSizes contents)) -- 1598415
  print $ sum (map ribbonLength (presentSizes contents)) -- 3812909
