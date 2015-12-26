import System.IO

puzzle = openFile "puzzles/day01.txt" ReadMode >>= hGetContents

solve n [] = n
solve n ('(' : xs) = solve (n + 1) xs
solve n (')' : xs) = solve (n - 1) xs

main = puzzle >>= print . solve 0
