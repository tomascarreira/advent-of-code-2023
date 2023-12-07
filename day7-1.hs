import qualified Text.Parsec as P
import Data.List
import qualified Data.Map.Strict as M
import Data.Char

type Card = Char 
type Hand = [Card]

main :: IO ()
main = do
  contents <- getContents
  print $ totalWinnings $ map parseLine (lines contents)

totalWinnings :: [(Hand, Int)] -> Int
totalWinnings xs =  foldl (\acc(i, (_, bid)) -> acc + i * bid) 0 (zip [1,2..] (sortBy compareHandsWithBid xs))

compareHandsWithBid :: (Hand, Int) -> (Hand, Int) -> Ordering
compareHandsWithBid (a,_) (b, _) = case compareType a b of
  EQ -> compareCards a b
  LT -> LT
  GT -> GT

compareCards :: Hand -> Hand -> Ordering
compareCards (ca:ar) (cb:br) = let comp = compare (cardToInt ca) (cardToInt cb) in
  case comp of
  EQ -> compareCards ar br
  LT -> LT
  GT -> GT

cardToInt :: Card -> Int
cardToInt c = if isDigit c then digitToInt c else
  case c of
  'A' -> 14
  'K' -> 13
  'Q' -> 12
  'J' -> 11
  'T' -> 10
  

compareType :: Hand -> Hand -> Ordering
compareType a b = 
  let a_cards = toTypeInt $ sort $ M.elems $ countCards a in
  let b_cards = toTypeInt $ sort $ M.elems $ countCards b in
  compare a_cards b_cards

toTypeInt :: [Int] -> Int
toTypeInt [1, 1, 1, 1, 1] = 1
toTypeInt [1, 1, 1, 2] = 2
toTypeInt [1, 2, 2] = 3
toTypeInt [1, 1, 3] = 4
toTypeInt [2, 3] = 5
toTypeInt [1, 4] = 6
toTypeInt [5] = 7


countCards :: Hand -> M.Map Card Int
countCards hand = M.fromListWith (+) $ zip hand [1,1..]

parseLine :: String -> (Hand, Int) 
parseLine line = let Right result = P.parse parser "line" line in result

parser :: P.Parsec String () (Hand, Int)
parser = do
  hand <- P.manyTill P.alphaNum P.space
  bid <- P.many1 P.digit
  return (hand, read bid) 
