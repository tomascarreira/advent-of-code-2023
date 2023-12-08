import qualified Text.Parsec as P
import qualified Data.Map.Strict as M

type Node = (String, (String, String))

main :: IO ()
main = do
  contents <- getContents
  print $ steps $ parseInput contents

steps :: (String, [Node]) -> Int
steps (instr, nodes) = let startingPoints = getStartiongPoints $ map fst nodes in foldl lcm 1 (map (steps' (cycle instr) (M.fromList nodes) 0) startingPoints)

getStartiongPoints :: [String] -> [String]
getStartiongPoints = filter (\n -> last n == 'A') 

steps' :: [Char] -> M.Map String (String, String) -> Int -> String -> Int
steps' (dir:rest) network n visiting = if last visiting == 'Z' then n else case dir of
  'L' -> let (l, r) = network M.! visiting in steps' rest network (n+1) l
  'R' -> let (l, r) = network M.! visiting in steps' rest network (n+1) r

-- steps' :: [Char] -> M.Map String (String, String) -> Int -> [String] -> Int
-- steps' (dir:rest) network n visiting = let (finished, nexts) = unzip $ map (step dir network) visiting in if and finished then n else steps' rest network (n+1) nexts

-- step :: Char -> M.Map String (String, String) -> String -> (Bool, String)
-- step dir network visiting = case dir of
--   'L' -> let (l, r) = network M.! visiting in (last visiting == 'Z', l)
--   'R' -> let (l, r) = network M.! visiting in (last visiting == 'Z', r)

parseInput :: String -> (String, [Node])
parseInput input = let Right result = P.parse parser "input" input in result

parser :: P.Parsec String () (String, [Node])
parser = do
  instr <- P.many1 P.letter
  P.newline
  P.newline
  nodes <- P.many1 $ do
    node <- P.many1 P.alphaNum
    P.string " = ("
    left <- P.many1 P.alphaNum
    P.string ", "
    right <- P.many1 P.alphaNum
    P.string ")\n"
    return (node, (left, right))
  return (instr, nodes)
