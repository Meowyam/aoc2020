import Data.List

-- first sort
-- then add first and last

convertInt :: [String] -> [Int]
convertInt = map read

searchTwo :: (Ord a, Num a) => [a] -> [a] -> a
searchTwo (l:ls) (r:rs)
  | ((l + r) < 2020) = searchTwo ls (r:rs)
  | ((l + r) > 2020) = searchTwo (l:ls) rs
  | otherwise = (l * r)

-- get possible combinations
-- filter sums

combiThree ls =
  [[x, y, z] | x <- ls, y <- ls, z <- ls, x < y, y < z]

filterThree ls =
  foldr (*) 1 $ concat $ filter (\x -> sum x == 2020) ls

main = do
  input <- fmap lines (readFile "input.txt")
  let format = sort $ convertInt input 
  let resultsTwo = searchTwo format (reverse format)
  putStrLn "product of the two numbers"
  print resultsTwo
  putStrLn "product of the three numbers"
  let resultsThree = filterThree (combiThree format)
  print resultsThree

