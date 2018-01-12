main :: IO()
main = do
  print $ gameOfLife [(0,1),(1,2),(2,0),(2,1),(2,2)]

gameOfLife :: [(Int, Int)] -> [(Int, Int)]
gameOfLife state = unique survivors
  where
    unique :: [(Int, Int)] -> [(Int, Int)]
    unique = uniqueHelper []
      where
        uniqueHelper :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
        uniqueHelper seen [] = seen
        uniqueHelper seen (x:rest)
          | x `elem` seen = uniqueHelper seen rest
          | otherwise = uniqueHelper (seen ++ [x]) rest

    survivors :: [(Int, Int)]
    survivors = willSurvive positions

    positions :: [(Int, Int)]
    positions = [(i+di, j+dj) | (i, j) <- state, di <- [-1..1], dj <- [-1..1]]

    willSurvive :: [(Int, Int)] -> [(Int, Int)]
    willSurvive xs = [pos | pos <- xs, liveOrDie pos]

    liveOrDie :: (Int, Int) -> Bool
    liveOrDie (x, y)
      | length neighbours == 3 || length neighbours == 4 = True
      | otherwise = False
        where
          neighbours :: [(Int, Int)]
          neighbours = [pair | pair <- state, check pair]

          check :: (Int, Int) -> Bool
          check (i, j) = i >= x - 1 && i <= x + 1 && j >= y - 1 && j <= y + 1
