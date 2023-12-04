import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ winningPoints $ lines contents

winningPoints :: [String] -> [Int]
winningPoints lines = map (\(winNums, nums) -> winningNumbers winNums nums) (map parseLine lines)  

parseLine :: String -> ([Int], [Int])
parseLine line = let Right res = P.parse parser "" line in res

winningNumbers :: [Int] -> [Int] -> Int
winningNumbers winNums nums = let l = length (filter (\e -> elem e winNums) nums) in 
  if l == 0 then 0 else 2 ^ (l - 1)

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
