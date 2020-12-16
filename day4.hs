import Data.List
import qualified Data.List.Split as Split
import qualified Data.Set as Set

clean :: String -> [[String]]
clean = map (map (take 3)) . map (Split.split . Split.dropFinalBlank . Split.dropDelims $ Split.oneOf "\n ") . Split.splitOn "\n\n"

mustFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

contains x y = all (`Set.member` Set.fromList x) y

checkContains :: [[String]] -> [Bool]
checkContains [] = []
checkContains (x:xs) =
  contains x mustFields : checkContains xs

main = do
  input <- readFile "input4.txt"
  print $ clean input
  print $ sum $ map fromEnum $ checkContains (clean input)
