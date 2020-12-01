import Data.List

-- first sort
-- then add first and last

convertInt :: [String] -> [Int]
convertInt = map read

search :: (Ord a, Num a) => [a] -> [a] -> a
search (l:ls) (r:rs)
  | ((l + r) < 2020) = search ls (r:rs)
  | ((l + r) > 2020) = search (l:ls) rs
  | otherwise = (l * r)

main = do
  input <- fmap lines (readFile "input.txt")
  let format = sort $ convertInt input 
  let results = search format (reverse format)
  print results
