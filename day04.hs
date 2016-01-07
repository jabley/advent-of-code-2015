import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)
import System.IO

--We read the value in from the file rather than hardcode.
puzzle = openFile "puzzles/day04.txt" ReadMode >>= hGetContents

--We have examples of how this function works in the question, so play in the REPL to get this.
h key = unpack . encode . hash . pack . (key ++) . show

-- predicate to see if it starts with the correct number of leading zeroes
isCoin n = (replicate n '0' ==) . take n

mine key = map (h key) [1 ..]

main = puzzle >>= print . head . filter (isCoin 5 . snd) . zip [1 ..] . mine
