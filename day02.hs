import Control.Monad
import System.IO

-- They have a list of the dimensions (length l, width w, and height h) of each present
puzzle = liftM lines (openFile "puzzles/day02.txt" ReadMode >>= hGetContents)

dims :: String -> (Int, Int, Int)
dims d = (read l, read w, read h) where
    (l, rest)  = span (/= 'x') d
    (w, rest') = span (/= 'x') (tail rest)
    h          = tail rest'

sides (l, w, h) = [ (l, w), (w, h), (h, l) ]

area (a, b) = a * b

perimeter (a, b) = 2 * (a + b)

paper ds = (2 * sum ss) + minimum ss where
    ss = map area (sides ds)

ribbon ds = minimum ps + volume ds where
    ps = map perimeter (sides ds)

volume (l, w, h) = l * w * h

totalPaper = sum . map (paper . dims)

totalVolume = sum . map (ribbon . dims)

main = do
    p <- puzzle
    print $ totalPaper p
    print $ totalVolume p
