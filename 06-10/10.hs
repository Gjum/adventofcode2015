import System.IO

main = do
  let num = "1113122113"
  --print $ lookAndSay num
  --print $ "311311222113" == lookAndSay num
  let after40 = (doTimes 40 lookAndSay) num
  print $ length after40 -- 360154
  print $ length $ (doTimes 10 lookAndSay) after40 -- 5103798

doTimes n fun = (foldr (.) id (replicate n fun))

lookAndSay num = foldr lookAndSayStep [] num

lookAndSayStep :: Char -> [Char] -> [Char]
lookAndSayStep c [] = ['1',c]
lookAndSayStep c (n:cn:ra) =
  if c == cn
    then (inc n) ++ (cn:ra)
    else '1':c:n:cn:ra

inc :: Char -> [Char]
inc n = show ((read [n] :: Int) + 1)
