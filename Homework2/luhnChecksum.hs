main :: IO()
main = do
  print $ calcLuhnChecksum 7992739871

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits num = getDigits (num `div` 10)++[num `mod` 10]

doubleEvenPositions :: [Int] -> [Int]
doubleEvenPositions xs = map doubleIfEven (zip [1..] xs)
  where
  doubleIfEven :: (Int, Int) -> Int
  doubleIfEven (i, j) = if even i then j * 2 else j

sumDigits :: Int -> Int
sumDigits num = sum (getDigits num)

sumDigitsInArray :: [Int] -> [Int]
sumDigitsInArray xs = map sumDigits xs

sumArray :: [Int] -> Int
sumArray xs = sum xs

getLastDigit :: Int -> Int
getLastDigit num = num `mod` 10

calcLuhnChecksum :: Int -> Int
calcLuhnChecksum = getLastDigit . reduceAndMultiply . prepareArray
  where
  prepareArray = sumDigitsInArray . doubleEvenPositions . getDigits
  reduceAndMultiply = (\x -> 9 * x) . sumArray
