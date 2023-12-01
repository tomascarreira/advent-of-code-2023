import Data.Char
import Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ calibrationValues $ lines contents

calibrationValues :: [String] -> [Int]
calibrationValues  = map (read . firstAndLast . parseLine)

firstAndLast :: String -> String
firstAndLast [a] = [a,a]
firstAndLast [a, b] = [a,b]
firstAndLast (a:b:rest) = firstAndLast (a:rest)

parseLine :: String -> String
parseLine = filter isDigit
