import qualified Data.Map.Strict as M
import Data.List (transpose, subsequences)

main :: IO ()
main = do
  contents <- getContents
  let image = toMap  (concat $ lines contents) (createIndexes $ lines contents) (createIndexes $ transpose $ lines contents) 
  let galaxies = getGalaxies image
  print $ sum $ map (shortestsPath image)  $ pair galaxies

createIndexes :: [String] -> [Int]
createIndexes lines = createIndexes' [] 0 lines 

createIndexes' :: [Int] -> Int -> [String] -> [Int]
createIndexes' acc _ [] = reverse acc
createIndexes' acc n (x:xs) = if '#' `elem` x then createIndexes' (n:acc) (n+1) xs else createIndexes' (n:acc)(n+1000000) xs

pair :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pair galaxies = pair' [] galaxies

pair' :: [((Int, Int), (Int, Int))] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
pair' acc [] = acc
pair' acc (g:gs) = pair' (map (g,) gs ++ acc) gs

shortestsPath :: M.Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> Int
shortestsPath image ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

getGalaxies :: M.Map (Int, Int) Char -> [(Int, Int)]
getGalaxies image = map fst $ filter (\(_, c) -> c == '#') $ M.assocs image

toMap :: String -> [Int] -> [Int] -> M.Map (Int, Int) Char
toMap image xIndex yIndex = M.fromList $ zip [(x, y) | x <- xIndex, y <- yIndex] $ image 
