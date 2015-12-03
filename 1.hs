import System.IO

delta '(' = 1
delta ')' = -1

basement (-1, p:r) = 0
basement (l, p:r) = 1 + basement ((l + delta p), r)

main = do
  withFile "1.in" ReadMode (\handle -> do
    contents <- hGetContents handle
    print $ sum (map delta contents) -- 232
    print $ basement (0, contents) -- ???
    )
