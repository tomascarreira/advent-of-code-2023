import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ scratchExplosion $ winningPoints $ lines contents

scratchExplosion :: [Int] -> Int
scratchExplosion xs = scratchExplosion' 0 (take (length xs) [1,1..1]) xs 

scratchExplosion' :: Int -> [Int] -> [Int] -> Int
scratchExplosion' acc [] [] = acc
scratchExplosion' acc (c:cs) (p:ps) = scratchExplosion' (acc+c) (addCopies p c cs) ps

addCopies :: Int -> Int -> [Int] -> [Int]
addCopies p c cs = addCopies' [] p c cs

addCopies' :: [Int] -> Int -> Int -> [Int] -> [Int]
addCopies' acc 0 _ cs = acc ++ cs
addCopies' acc p cop (c:cs) = addCopies' (acc ++ [c+cop]) (p-1) cop cs

winningPoints :: [String] -> [Int]
winningPoints lines = map (\(winNums, nums) -> winningNumbers winNums nums) (map parseLine lines)  

parseLine :: String -> ([Int], [Int])
parseLine line = let Right res = P.parse parser "" line in res

winningNumbers :: [Int] -> [Int] -> Int
winningNumbers winNums nums = length (filter (\e -> elem e winNums) nums)

parser :: P.Parsec String () ([Int], [Int])
parser = do
  P.string "Card"
  P.many1 P.space
  P.many1 P.digit
  P.string ":"
  P.many1 P.space
  winNums <- P.endBy1 (P.many1 P.digit) (P.many1 P.space)
  P.string "|"
  P.many1 P.space
  nums <- P.sepBy1 (P.many1 P.digit) (P.many1 P.space)
  return (map read winNums, map read nums)
