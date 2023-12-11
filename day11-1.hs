import qualified Data.Map.Strict as M
import Data.List (transpose, subsequences)

main :: IO ()
main = do
  contents <- getContents
  let realImage = expandSpace $ lines contents
  let galaxies = getGalaxies realImage
  print $ sum $ map (shortestsPath realImage)  $ pair galaxies

pair :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pair galaxies = pair' [] galaxies

pair' :: [((Int, Int), (Int, Int))] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
pair' acc  [] = acc
pair' acc (g:gs) = pair' (map (g,) gs ++ acc) gs

shortestsPath :: M.Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> Int
shortestsPath image ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

getGalaxies :: M.Map (Int, Int) Char -> [(Int, Int)]
getGalaxies image = map fst $ filter (\(_, c) -> c == '#') $ M.assocs image

expandSpace :: [String] -> M.Map (Int, Int) Char
expandSpace image = let newImage = transpose $ addSpaces $ transpose $ addSpaces image in M.fromList $ zip [(x, y) | x <- [0..length newImage - 1], y <- [0..length (head newImage) - 1]] $ concat newImage 

addSpaces :: [String] -> [String]
addSpaces = addSpaces' []

addSpaces' :: [String] -> [String] -> [String]
addSpaces' acc [] = reverse acc
addSpaces' acc (x:xs) = if '#' `elem` x then addSpaces' (x:acc) xs else addSpaces' (x:replicate (length x) '.':acc) xs
