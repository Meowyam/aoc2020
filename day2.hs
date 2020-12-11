import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Char
import Control.Applicative
import Control.Monad

getRange :: [Char] -> [[Char]]
getRange =  map (filter isDigit) . Split.splitOn "-"

rangeToInt :: [Char] -> [Int]
rangeToInt ls = map read (getRange ls)

getPass :: [Char] -> [Char]
getPass = concat . map (filter isAlpha) . Split.splitOn ":"

format :: [[Char]] -> [([Int], [Char])]
format = (zip . fmap rangeToInt) <*> (fmap getPass)

countChar :: Eq a1 => (a2, [a1]) -> Int
countChar tup =
  length $ filter (==(head x)) (tail x)
    where
      x = snd tup

checkRange tup =
  n >= (head x) && n <= (last x) 
    where x = fst tup
          n = countChar tup

checkPos tup
  | (f /= g) = True
  | otherwise = False
    where x = fst tup
          (n:ns) = snd tup
          f = (n == (ns !! ((head x)-1)))
          g = (n == (ns !! ((last x)-1)))

main = do
  input <- fmap lines (readFile "input2.txt")
  print $ sum $ map fromEnum (fmap checkRange (format input))
  print $ sum $ map fromEnum (fmap checkPos (format input))

