import System.IO

main = do
  ls <- lines <$> readFile "05.in"
  print $ length $ niceWords1 ls -- 238
  print $ length $ niceWords2 ls -- 69

------------ part 1 ------------

niceWords1 ls = [word | word <- ls, 3 <= vowels word, hasDouble word, noabcdpqxy word]

vowels ('a':rs) = 1 + vowels rs
vowels ('e':rs) = 1 + vowels rs
vowels ('i':rs) = 1 + vowels rs
vowels ('o':rs) = 1 + vowels rs
vowels ('u':rs) = 1 + vowels rs
vowels (_:rs) = vowels rs
vowels [] = 0

hasDouble (x:y:r) =
  if x == y
    then True
    else hasDouble (y:r)
hasDouble _ = False

noabcdpqxy ('a':'b':r) = False
noabcdpqxy ('c':'d':r) = False
noabcdpqxy ('p':'q':r) = False
noabcdpqxy ('x':'y':r) = False
noabcdpqxy (_:r) = noabcdpqxy r
noabcdpqxy [] = True

------------ part 2 ------------

niceWords2 ls = [word | word <- ls, hasBetween word, twoPairs [] word]

hasBetween (x1:y:x2:r) =
  if x1 == x2
    then True
    else hasBetween (y:x2:r)
hasBetween _ = False

twoPairs :: [(Char,Char)] -> [Char] -> Bool
twoPairs pairs (x:y:r) =
  if (x,y) `elem` pairs
    then True
  else twoPairs ((x,y):pairs) (r) || twoPairs (pairs) (y:r)
twoPairs _ _ = False
