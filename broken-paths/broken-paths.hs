main :: IO ()
main = putStrLn$fixPaths [["00001000","00010000"],["00000100","00000000"]]

rotational :: [(Int, Int)]
rotational=zip[-1,0,1,1,1,0,-1,-1][-1,-1,-1,0,1,1,1,0]

getIndex[]_ _= -1
getIndex(e:l)x i|x==e=i|1>0=getIndex l x i+1

l?x=getIndex l x 0


fixPaths :: [[String]] -> [[String]]
fixPaths nodes =
    until (\nMx -> all(==0).concat.implicationMx $ nMx) id nodes


implicationMx nodes =
    let dim = length nodes - 1
        i#j = nodes!!i!!j
        implications i j =
            let thisnode = i#j
                locs = [[x,y]|[x,y]<-mapM(\w->[0..w])[dim,dim],x>i-2&&x<i+2&&y>j-2&&y<j+2]
            in  sum[1|[x,y]<-locs,
                    let ld = (x-i,y-j)
                        ld' = (i-x,j-y)
                    in  ld/=ld'&& ('1'==thisnode!!(rotational?ld)) /= ('1'==x#y!!(rotational?ld'))
                ]
    in  [implications row col | [row,col] <- mapM(\x->[0..x])[dim,dim]]


satisfyMaxImpl :: [[String]] -> [[String]]
satisfyMaxImpl nodes
    | all(==0).concat.implicationMx $ nodes = nodes
    | 1>0 =
        let dim = length nodes - 1
            i#j = nodes!!i!!j
            iMx = implicationMx nodes
            cartesian = mapM(\w->[0..w])[dim,dim]
            maxImpMxCoord [x,y] [a,b]|iMx!!x!!y>iMx!!a!!b=[x,y]|1>0=[a,b]
            [maxx,maxy] = foldr1 maxImpMxCoord cartesian
            locs = [[x,y]|[x,y]<-cartesian,x>i-2&&x<i+2&&y>j-2&&y<j+2]


{-

nodes = [["00001000","00010000"],["00000100","00000000"]]

dim = 2

[
    row = 0
    [
        col = 0

        i = 0, j = 0
        thisnode = "00001000"
        locs = [(0,0), (0,1), (1,0), (1,1)]

    ]
]

-}
