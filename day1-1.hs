import Data.Char
import Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ calibrationValues $ lines contents

calibrationValues :: [String] -> [Int]
calibrationValues  = map (read . sumFirstAndLast . parseLine)

sumFirstAndLast :: String -> String
sumFirstAndLast [a] = [a,a]
sumFirstAndLast [a, b] = [a,b]
sumFirstAndLast (a:b:rest) = sumFirstAndLast (a:rest)

parseLine :: String -> String
parseLine = filter isDigit
