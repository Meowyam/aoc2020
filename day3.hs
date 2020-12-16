import Data.List

move [] _ _ _ = []
move (y:ys) x n s = (y !! i) : (move (drop s ys) (x+n) n s)
  where i = x `mod` (length y)

count str c
  = length $ filter (==c) str

slopes y x k [] = []
slopes y x k (n:ns) =
  (count (move y x n k) '#') : slopes y x k ns

main = do
  input <- fmap lines (readFile "input3.txt")
  print $ move input 0 3 0
  print $ count (move input 0 3 0) '#'
  print $ foldr (*) 1 $ slopes input 0 0 [1,3,5,7] ++ slopes input 0 1 [1]
