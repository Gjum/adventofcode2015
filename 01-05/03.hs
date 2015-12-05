import System.IO
import Data.Set as Set

move '^' (x,y) = (x  ,y+1)
move 'v' (x,y) = (x  ,y-1)
move '>' (x,y) = (x+1,y  )
move '<' (x,y) = (x-1,y  )

part1 [] pos set = set
part1 (dir:newdata) oldpos set = part1 newdata newpos newset
  where
    newpos = move dir oldpos
    newset = insert newpos set

part2 [] posa posb set = set
part2 (dira:dirb:newdata) oldposa oldposb set = part2 newdata newposa newposb newset
  where
    newposa = move dira oldposa
    newposb = move dirb oldposb
    newset = insert newposa (insert newposb set)

main = do
  withFile "3.in" ReadMode (\handle -> do
    contents <- hGetContents handle
    print $ size $ part1 contents (0,0) (singleton (0,0)) -- 2572
    print $ size $ part2 contents (0,0) (0,0) (singleton (0,0)) -- 2631
    )
