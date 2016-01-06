import Data.Set
import System.IO

puzzle = openFile "puzzles/day03.txt" ReadMode >>= hGetContents

-- visited :: currentPosition -> history -> directions -> setOfLocations
visited :: (Int, Int) -> [(Int, Int)] -> String -> Set (Int, Int)
visited (x, y) history [] = fromList ((x, y) : history)
visited (x, y) history ('^' : xs) = visited (x, y+1) ((x, y) : history) xs
visited (x, y) history ('v' : xs) = visited (x, y-1) ((x, y) : history) xs
visited (x, y) history ('>' : xs) = visited (x+1, y) ((x, y) : history) xs
visited (x, y) history ('<' : xs) = visited (x-1, y) ((x, y) : history) xs

main = puzzle >>= print . length . visited (0,0) []
