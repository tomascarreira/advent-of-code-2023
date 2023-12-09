import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ map (prediction . reverse. parseLine) (lines contents)

prediction :: [Int] -> Int
prediction history = prediction' [history] history

prediction' :: [[Int]] -> [Int] -> Int
prediction' acc history = let xs = map (\(a, b) -> b - a) (zip history (tail history)) in
  if all (== 0) xs then goUp (xs:acc) else prediction' (xs:acc) xs

goUp :: [[Int]] -> Int
goUp xs = sum $ map last xs
 
parseLine :: String -> [Int]
parseLine line = let Right res = P.parse parser "line" line in res

parseNumber :: P.Parsec String () Int
parseNumber = do
  sign <- P.option "" (P.many1 (P.char '-'))
  number <- P.many1 P.digit
  return (read (sign++number)) 

parser :: P.Parsec String () [Int]
parser = P.sepBy1 parseNumber P.spaces
