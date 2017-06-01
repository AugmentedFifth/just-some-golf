import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let (x, y) = (read $ args !! 0, read $ args !! 1)
    mapM_ putStrLn (x#y)

{----- Golfing material below -----}

r=[0..9]
x#y=['|':(r>>=(\i->[last$'_':['*'|elem(i-x,j-y)$zip[1,2,0,1,2][0,1,2,2,2]],'|']))|j<-r]
