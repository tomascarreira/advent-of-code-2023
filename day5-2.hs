import qualified Text.Parsec as P

type MapRange = [(Int, Int, Int)]
type Seed = Int

main :: IO ()
main = do
  contents <- getContents
  let Right res = P.parse parser "input" contents in
    print $ lowestLocationNumber res

lowestLocationNumber :: ([Seed], [MapRange]) -> Int
lowestLocationNumber (seeds, mapRanges) = minimum (mapAllTheWay (rangesToList seeds) mapRanges)

rangesToList :: [Seed] -> [Int]
rangesToList ranges = concat $ rangesToList' [] ranges

rangesToList' :: [[Int]] -> [Int] -> [[Int]]
rangesToList' acc [] = acc
rangesToList' acc (start:len:rest) = rangesToList' ([start..(start+len - 1)]:acc) rest

mapAllTheWay :: [Int] -> [MapRange] -> [Int]
mapAllTheWay xs [] = xs
mapAllTheWay xs (mapRange:rest) = mapAllTheWay (map (\e -> doMap e mapRange) xs) rest

doMap :: Int -> MapRange -> Int
doMap n [] = n
doMap n ((to, from, len):rest) = if n >= from && n < from + len then to + (n - from) else doMap n rest

mapRangeParser :: P.Parsec String () (Int, Int, Int)
mapRangeParser = do
  toStart <- P.many1 P.digit
  P.space
  fromStart <- P.many1 P.digit
  P.space
  len <- P.many1 P.digit
  P.newline
  return (read toStart, read fromStart, read len)

parser :: P.Parsec String () ([Seed], [MapRange])
parser = do
  P.string "seeds: "
  seeds <- P.sepEndBy1 (P.many1 P.digit) P.space
  P.newline
  P.string "seed-to-soil map:\n"
  seedToSoil <- P.many1 mapRangeParser
  P.newline
  P.string "soil-to-fertilizer map:\n"
  soilToFertilizer <- P.many1 mapRangeParser
  P.newline
  P.string "fertilizer-to-water map:\n"
  fertilizerToWater <- P.many1 mapRangeParser
  P.newline
  P.string "water-to-light map:\n"
  waterToLight <- P.many1 mapRangeParser
  P.newline
  P.string "light-to-temperature map:\n"
  lightToTemperature <- P.many1 mapRangeParser
  P.newline
  P.string "temperature-to-humidity map:\n"
  temperatureToHumidity <- P.many1 mapRangeParser
  P.newline
  P.string "humidity-to-location map:\n"
  humidityToLocation <- P.many1 mapRangeParser
  return (map read seeds, [seedToSoil,soilToFertilizer,fertilizerToWater,waterToLight,lightToTemperature, temperatureToHumidity, humidityToLocation])
