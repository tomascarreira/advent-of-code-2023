import qualified Text.Parsec as P
import Data.Map.Strict as M

type Node = (String, (String, String))

main :: IO ()
main = do
  contents <- getContents
  print $ steps $ parseInput contents

steps :: (String, [Node]) -> Int
steps (instr, nodes) = steps' (cycle instr) (M.fromList nodes) 0 "AAA"

steps' :: [Char] -> M.Map String (String, String) -> Int -> String -> Int
steps' (dir:rest) network n visiting = if visiting == "ZZZ" then n else case dir of
  'L' -> let (l, r) = network ! visiting in steps' rest network (n+1) l
  'R' -> let (l, r) = network ! visiting in steps' rest network (n+1) r

parseInput :: String -> (String, [Node])
parseInput input = let Right result = P.parse parser "input" input in result

parser :: P.Parsec String () (String, [Node])
parser = do
  instr <- P.many1 P.letter
  P.newline
  P.newline
  nodes <- P.many1 $ do
    node <- P.many1 P.letter
    P.string " = ("
    left <- P.many1 P.letter
    P.string ", "
    right <- P.many1 P.letter
    P.string ")\n"
    return (node, (left, right))
  return (instr, nodes)
