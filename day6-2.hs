import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ numberOfWaysToBeatRecord $ parse contents

numberOfWaysToBeatRecord :: (Int, Int) -> Int
numberOfWaysToBeatRecord race = length $ waysToBeat race  

waysToBeat :: (Int, Int) -> [Int]
waysToBeat (time, distance) = [speed | speed <- [0..time], speed * (time - speed) > distance]

parse :: String -> (Int, Int)
parse contents = let Right res = P.parse parser "input" contents in res

parser :: P.Parsec String () (Int, Int)
parser = do
  P.string "Time:"
  P.spaces
  times <- P.sepEndBy1 (P.many1 P.digit) P.spaces
  P.string "Distance:"
  P.spaces
  distances <- P.sepEndBy1 (P.many1 P.digit) P.spaces
  return (read $ concat times, read $ concat distances)
