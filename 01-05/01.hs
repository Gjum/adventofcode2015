import System.IO

delta '(' = 1
delta ')' = -1

basement (-1, p:r) = 0
basement (l, p:r) = 1 + basement ((l + delta p), r)

main = do
  contents <- readFile "01.in"
  print $ sum (map delta contents) -- 232
  print $ basement (0, contents) -- 1783
