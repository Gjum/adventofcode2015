main = do
  containers <- map read <$> lines <$> readFile "17.in"
  let combinations = combine 150 containers
  print $ length $ combinations -- 654
  let minComLen = foldr (\com num -> min num (length com)) (length containers) combinations
  print $ length $ [com | com <- combinations, minComLen == length com] -- 57
  print $ "minimum number of containers: " ++ (show minComLen) -- 4

combine 0 [c] = [[]]
combine total [c] = if total == c then [[c]] else []
combine total (c:cs) = without ++ with
  where without = combine total cs
        with = map (\cs -> c:cs) $ combine (total-c) cs
