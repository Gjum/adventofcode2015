import System.IO

totalArea :: (Num a, Ord a) => a -> a -> a -> a
totalArea l w h = minimum areas + sum areas
  where areas = [product [x, y] | (x,y) <- [(l,w),(w,h),(l,h)] ]

--main = print $ totalArea 2 2 2

splitter :: Char -> [Char] -> [[Char]]
splitter c [] = []
splitter c [a] = [[a]]
splitter c (a:r) =
  if c == a
    then [] : (s:ts)
    else (a:s) : ts
  where s:ts = splitter c r

main = do
  withFile "2.in" ReadMode (\handle -> do
    contents <- hGetContents handle
    print $ splitter '\n' contents)
