import Data.List

move [] _ = []
move (y:ys) x = (y !! i) : (move ys (x+3))
  where i = x `mod` (length y)

count str c
  = length $ filter (==c) str

main = do
  input <- fmap lines (readFile "input3.txt")
  -- how high
  print $ length input
  print $ move input 0
  print $ count (move input 0) '#'
