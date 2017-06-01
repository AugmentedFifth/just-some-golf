import System.Environment


main :: IO ()
main = do
    args <- getArgs
    let n = read $ head args
    print $ f n


f n=sum$(n/)<$>[1..n]
