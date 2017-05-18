-- main :: IO ()
main = print$f[["00000100","00010000","00000000"],["00000001","10000000","00010000"],["00000000","00000000","10000001"]]


-- r :: [(Int, Int)]
r=zip[-1,0,1,1,1,0,-1,-1][-1,-1,-1,0,1,1,1,0]


-- g :: (Integral i, Eq a) => i -> [a] -> a -> i
g _[]_= -1
g i(e:l)x|x==e=i|1>0=g(i+1)l x

l?x=g 0 l x


-- d :: Integral i => i -> [a] -> i -> a -> [a]
d _[]_ _=[]
d i(e:l)t v|i==t=v:l|1>0=e:d(i+1)l t v


-- c :: Integral i => [a] -> i -> a -> [a]
c=d 0

-- q :: Integral i => [[String]] -> [[i]]


f :: [[String]] -> [[String]]
f n
    | all(==0).concat.q$n=n
    | 1>0=f$foldr(\p b->s b [maxx,maxy] p)n$maxx!maxy
        where
            h=length n-1
            t=[0..h]
            e=mapM(\w->[0..w])[h,h]
            i!j=[[x,y]|[x,y]<-e,x>i-2&&x<i+2&&y>j-2&&y<j+2]
            q o=let
                i#j=o!!i!!j
                    in  [[sum[1|[x,y]<-row!col,
                                    let ld=(x-row,y-col)
                                        ld'=(row-x,col-y)
                                    in  ld/=ld'&& ('1'==row#col!!(r?ld)) /= ('1'==x#y!!(r?ld'))
                                ] | col <- t] | row <- t]
            maxImpMxCoord[x,y][a,b]|q n!!x!!y>q n!!a!!b=[x,y]|1>0=[a,b]
            [maxx,maxy]=foldr1 maxImpMxCoord e
            s nodes'[x1,y1][x2,y2]
                |x1==x2&&y1==y2=
                    nodes'
                |'1'==nodes'!!x1!!y1!!(r?(x2-x1,y2-y1))=
                    z '0'
                |'1'==nodes'!!x2!!y2!!(r?(x1-x2,y1-y2))=
                    z '1'
                |1>0=
                    nodes'
                where
                 z char=c nodes' x1 (c (nodes'!!x1) y1 (c (nodes'!!x1!!y1) (r?(x2-x1,y2-y1)) char))




{-

r,g,c,d,q,n,f,t,z,b,s,h,

?,#,!,

-}
