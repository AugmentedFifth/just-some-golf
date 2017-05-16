import System.Environment -- Only necessary for test harness

import Data.List
k x y=unlines[[toEnum$findIndices(elem(i,j))(scanl(\s _->filter(\(z,w)->z`elem`n&&w`elem`n)$(\(a,b)->[(a+c,b+d)|(c,d)<-zip [1,1,-1,-1,-2,-2,2,2] [2,-2,2,-2,1,-1,1,-1]])=<<s)[(x,y)]n)!!0+48|j<-n]|i<-n]where n=[0..7]


main :: IO ()
main = do
    args <- getArgs
    let readArgs = map read args
    let out = k (readArgs !! 0) (readArgs !! 1)
    putStr out
