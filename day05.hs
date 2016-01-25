import Control.Monad
import System.IO
import Data.List (group)

puzzle = liftM lines (openFile "puzzles/day05.txt" ReadMode >>= hGetContents)

pairs :: [Char] -> [[Char]]
pairs [] = []
pairs [_] = []
pairs (x:y:ys) = [x,y] : pairs (y : ys)

vowelCount :: [Char] -> Int
vowelCount = length . filter(`elem` "aeiou")

threeOrMoreVowels :: [Char] -> Bool
threeOrMoreVowels = (>2) . vowelCount

atLeastOneDouble :: [Char] -> Bool
atLeastOneDouble = any ((> 1) . length) . group

noBanned :: [Char] -> Bool
noBanned = not . any (`elem` ["ab", "cd", "pq", "xy"]) . pairs

(&&&) a b = (&&) <$> a <*> b

nice = threeOrMoreVowels &&& atLeastOneDouble &&& noBanned

main = do
    p <- puzzle
    print $ length $ filter nice p

