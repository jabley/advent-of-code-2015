import System.IO

puzzle = openFile "puzzles/day01.txt" ReadMode >>= hGetContents

solve n [] = n
solve n ('(' : xs) = solve (n + 1) xs
solve n (')' : xs) = solve (n - 1) xs

solve' (-1) i _ = i
solve' floor i ('(' : xs) = solve' (floor+1) (i+1) xs
solve' floor i (')' : xs) = solve' (floor-1) (i+1) xs

main = do
    p <- puzzle
    print $ solve 0 p
    print $ solve' 0 0 p
