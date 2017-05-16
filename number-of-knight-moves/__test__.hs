k x y =
    unlines (map (\i -> map (\j ->
        toEnum $ findIndices (elem (i, j))
    head (scanl
     (\ ps@(pos : poses) i ->
        filter (\ z -> fst z `elem` [0 .. 7] && snd z `elem` [0 .. 7])
          (concatMap
             (\ pos ->
                map (\(zpd1, zpd2) -> (fst pos + zpd1, snd pos + zpd2)) $
                  zip [1, 1, -1, -1, -2, -2, 2, 2] [2, -2, 2, -2, 1, -1, 1, -1])
             ps))
     [(x, y)]
     [0..9]) + 48) [0..7]) [0..7])