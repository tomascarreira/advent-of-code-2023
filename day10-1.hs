import qualified Data.Map.Strict as M
import Debug.Trace

data DIR = UP | RIGHT | DOWN | LEFT

main :: IO ()
main = do
  contents <- getContents
  let tiles = M.fromList $ zip [(x, y) | x <- [0..length (head (lines contents)) - 1], y <- [0..length (lines contents)- 1]] (concat $ lines contents)  
  let start = findS tiles 
  let startPipe = computeStartPipe  start tiles ['|', '-', 'L', 'J', '7', 'F']
  print $ div (loopSize start startPipe tiles)  2

loopSize :: (Int, Int) -> Char -> M.Map (Int, Int) Char -> Int
loopSize (x, y) '|' tiles = loopSize' 1 DOWN (x-1, y) (tiles M.! (x-1, y)) tiles
loopSize (x, y) '-' tiles = loopSize' 1 LEFT (x, y+1) (tiles M.! (x, y+1)) tiles
loopSize (x, y) 'L' tiles = loopSize' 1 DOWN (x-1, y) (tiles M.! (x-1, y)) tiles
loopSize (x, y) 'J' tiles = loopSize' 1 DOWN (x-1, y) (tiles M.! (x-1, y)) tiles
loopSize (x, y) '7' tiles = loopSize' 1 UP (x+1, y) (tiles M.! (x+1, y)) tiles
loopSize (x, y) 'F' tiles = loopSize' 1 LEFT (x, y+1) (tiles M.! (x, y+1)) tiles

loopSize' :: Int -> DIR -> (Int, Int) -> Char -> M.Map (Int, Int) Char -> Int
loopSize' n _ _ 'S' _ = n
loopSize' n UP (x, y) '|' tiles = loopSize' (n+1) UP (x+1,y) (tiles M.! (x+1, y)) tiles
loopSize' n DOWN (x, y) '|' tiles = loopSize' (n+1) DOWN (x-1,y) (tiles M.! (x-1, y)) tiles
loopSize' n LEFT (x, y) '-' tiles = loopSize' (n+1) LEFT (x,y+1) (tiles M.! (x, y+1)) tiles
loopSize' n RIGHT (x, y) '-' tiles = loopSize' (n+1) RIGHT (x,y-1) (tiles M.! (x, y-1)) tiles
loopSize' n UP (x, y) 'L' tiles = loopSize' (n+1) LEFT (x,y+1) (tiles M.! (x, y+1)) tiles
loopSize' n RIGHT (x, y) 'L' tiles = loopSize' (n+1) DOWN (x-1,y) (tiles M.! (x-1, y)) tiles
loopSize' n UP (x, y) 'J' tiles = loopSize' (n+1) RIGHT (x,y-1) (tiles M.! (x, y-1)) tiles
loopSize' n LEFT (x, y) 'J' tiles = loopSize' (n+1) DOWN (x-1,y) (tiles M.! (x-1, y)) tiles
loopSize' n DOWN (x, y) '7' tiles = loopSize' (n+1) RIGHT (x,y-1) (tiles M.! (x, y-1)) tiles
loopSize' n LEFT (x, y) '7' tiles = loopSize' (n+1) UP (x+1,y) (tiles M.! (x+1, y)) tiles
loopSize' n RIGHT (x, y) 'F' tiles = loopSize' (n+1) UP (x+1,y) (tiles M.! (x+1, y)) tiles
loopSize' n DOWN (x, y) 'F' tiles = loopSize' (n+1) LEFT (x,y+1) (tiles M.! (x, y+1)) tiles

computeStartPipe :: (Int, Int) -> M.Map (Int, Int) Char -> [Char]-> Char
computeStartPipe (x, y) tiles pipes = 'J'

findS :: M.Map (Int, Int) Char -> (Int, Int)
findS tiles = fst $ head $ filter (\(pos, c) -> c == 'S') (M.assocs tiles)
