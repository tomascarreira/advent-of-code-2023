import Data.Char
import qualified Text.Parsec as P

main :: IO ()
main = do
  contents <- getContents
  print $ sum $ calibrationValues $ lines contents

calibrationValues :: [String] -> [Int]
calibrationValues  = map (read . firstAndLast . parseLine)

firstAndLast :: String -> String
firstAndLast [a] = [a,a]
firstAndLast [a, b] = [a,b]
firstAndLast (a:b:rest) = firstAndLast (a:rest)

parseLine :: String -> String
parseLine line = reverse $ parseLine' "" line

parseLine' :: String -> String -> String
parseLine' acc "" = acc
parseLine' acc ('1':rest) = parseLine' ('1':acc) rest
parseLine' acc ('2':rest) = parseLine' ('2':acc) rest
parseLine' acc ('3':rest) = parseLine' ('3':acc) rest
parseLine' acc ('4':rest) = parseLine' ('4':acc) rest
parseLine' acc ('5':rest) = parseLine' ('5':acc) rest
parseLine' acc ('6':rest) = parseLine' ('6':acc) rest
parseLine' acc ('7':rest) = parseLine' ('7':acc) rest
parseLine' acc ('8':rest) = parseLine' ('8':acc) rest
parseLine' acc ('9':rest) = parseLine' ('9':acc) rest
parseLine' acc ('o':'n':'e':rest) = parseLine' ('1':acc) ('e':rest)
parseLine' acc ('t':'w':'o':rest) = parseLine' ('2':acc) ('o':rest)
parseLine' acc ('t':'h':'r':'e':'e':rest) = parseLine' ('3':acc) ('e':rest)
parseLine' acc ('f':'o':'u':'r':rest) = parseLine' ('4':acc) rest
parseLine' acc ('f':'i':'v':'e':rest) = parseLine' ('5':acc) ('e':rest)
parseLine' acc ('s':'i':'x':rest) = parseLine' ('6':acc) rest
parseLine' acc ('s':'e':'v':'e':'n':rest) = parseLine' ('7':acc) ('n':rest)
parseLine' acc ('e':'i':'g':'h':'t':rest) = parseLine' ('8':acc) ('t':rest)
parseLine' acc ('n':'i':'n':'e':rest) = parseLine' ('9':acc) ('e':rest)
parseLine' acc (c:rest) = parseLine' acc rest

-- parseLine :: String -> String
-- parseLine line =  let res = P.parse parser "line" line in
--   case res of
--     Right l -> concatMap spellOutToDigit (filter (\e -> not (length e == 1 && isAlpha (e!!0))) l)
--     Left e -> error $ show e

-- spellOutToDigit :: String -> String
-- spellOutToDigit "one" = "1"
-- spellOutToDigit "two" = "2"
-- spellOutToDigit "three" = "3"
-- spellOutToDigit "four" = "4"
-- spellOutToDigit "five" = "5"
-- spellOutToDigit "six" = "6"
-- spellOutToDigit "seven" = "7"
-- spellOutToDigit "eight" = "8"
-- spellOutToDigit "nine" = "9"
-- spellOutToDigit xs = xs

-- spelledOutDigit :: P.Parsec String () String
-- spelledOutDigit =
--   P.try (P.string "one") P.<|> 
--   P.try (P.string "two") P.<|>
--   P.try (P.string "three") P.<|>
--   P.try (P.string "four") P.<|>
--   P.try (P.string "five") P.<|>
--   P.try (P.string "six") P.<|>
--   P.try (P.string "seven") P.<|>
--   P.try (P.string "eight") P.<|>
--   P.try (P.string "nine")

-- parser :: P.Parsec String () [String]
-- parser = P.many1 $ P.try (P.count 1 P.digit P.<|> spelledOutDigit) P.<|> P.count 1 P.letter

