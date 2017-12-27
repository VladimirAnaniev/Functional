main :: IO()
main = do
  print "123"

gameOfLife :: [(Int, Int)] -> [(Int, Int)]
gameOfLife state = unique survivors
  where
    unique :: [(Int, Int)] -> [(Int, Int)]
    unique = uniqueHelper []
      where
        uniqueHelper seen [] = seen
        uniqueHelper seen (x:rest)
          | x `elem` seen = uniqueHelper seen rest
          | otherwise = uniqueHelper (seen ++ [x]) rest

    survivors = willSurvive [(i+di, j+dj) | (i, j) <- state, di <- [-1..1], dj <- [-1..1]]

    willSurvive :: [(Int, Int)] -> [(Int, Int)]
    willSurvive xs = map (\(_, pos) -> pos) (filter (\(lod, _) -> lod) (zip (map liveOrDie xs) xs))

    liveOrDie :: (Int, Int) -> Bool
    liveOrDie (x, y)
      | length neighbours == 3 || length neighbours == 4 = True
      | otherwise = False
        where
          neighbours = [(i, j) | (i, j) <- state, i >= x - 1 && i <= x + 1 && j >= y - 1 && j <= y + 1]
