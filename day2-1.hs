import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ possibleGamesId $ lines contents

possibleGamesId :: [String] -> [Int]
possibleGamesId lines = map fst (filter isPossible $ map parseLine lines)

isPossible :: (Int, [(Int, Int, Int)]) -> Bool
isPossible (_, xs) = all (\(r, g, b) -> r <= 12 && g <= 13 && b <= 14) xs

parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine line = let Right game = P.parse parser "line" line in
  (fst game, map countCubes (snd game))

countCubes :: [(Int, String)] -> (Int, Int, Int)
countCubes = foldl (\(r, g, b)(n, color) -> case color of
  "red" -> (r+n, g, b)
  "green" -> (r, g+n, b)
  "blue" -> (r, g, b+n)
  ) (0, 0, 0)

cubesParser :: P.Parsec String () (Int, String)
cubesParser = do
  P.many P.space
  n <- P.many1 P.digit
  P.many1 P.space
  cube <- P.string "blue" P.<|> P.string "red" P.<|> P.string "green"
  return (read n, cube)

subsetParser :: P.Parsec String () [(Int, String)]
subsetParser = do 
  s <- P.sepBy1 cubesParser $ P.char ','
  P.many $ P.char ';'
  P.many P.space
  return s

parser :: P.Parsec String () (Int, [[(Int, String)]])
parser = do
  P.string "Game"
  P.many1 P.space
  gameId <- P.many1 P.digit
  P.char ':'
  P.many1 P.space
  set <- P.many1 subsetParser
  return (read gameId, set)
  
