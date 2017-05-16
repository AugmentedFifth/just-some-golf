import System.Environment
import Data.List



main :: IO ()
main = do
    args <- getArgs
    let readArgs = map read args
    let out = k (readArgs !! 0) (readArgs !! 1)
    putStr out

k x y=unlines[[toEnum$findIndices(elem(i,j))(scanl(\s _->filter(\(z,w)->z`elem`n&&w`elem`n)$(\(a,b)->[(a+c,b+d)|(c,d)<-zip [1,1,-1,-1,-2,-2,2,2] [2,-2,2,-2,1,-1,1,-1]])=<<s)[(x,y)][0..9])!!0+48|j<-n]|i<-n]where n=[0..7]



{-

k x y =
    unlines (map (\i -> map (\j ->
        toEnum $ findIndices (elem (i, j))
    (scanl
        (\ps@(pos:_) i ->
        filter (\z -> fst z `elem` n && snd z `elem` n)
          (concatMap
             (\pos ->
                 map (\(zpd1, zpd2) -> (fst pos + zpd1, snd pos + zpd2)) $
                     zip [1, 1, -1, -1, -2, -2, 2, 2] [2, -2, 2, -2, 1, -1, 1, -1])
             ps))
    [(x, y)]
    [0 .. 9])
        !! 0 + 48) [0..7]) [0..7])
    where n = [0..7]

-}
