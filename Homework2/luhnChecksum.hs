main :: IO()
main = do
  print $ calcLuhnChecksum 7992739871

calcLuhnChecksum :: Int -> Int
calcLuhnChecksum = getLastDigit . arrSum
  where
    arrSum :: Int -> Int
    arrSum num = 9 * sum (arr num)

    arr :: Int -> [Int]
    arr num = [sumDigits x | x <- [double p | p <- zip [1..] (digits num)]]

    digits :: Int -> [Int]
    digits 0 = []
    digits num = digits (num `div` 10)++[num `mod` 10]

    double :: (Int, Int) -> Int
    double (i, j) = if even i then j * 2 else j

    sumDigits :: Int -> Int
    sumDigits num = sum (digits num)

    getLastDigit :: Int -> Int
    getLastDigit num = num `mod` 10
