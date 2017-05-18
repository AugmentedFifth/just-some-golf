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


-- f :: [[String]] -> [[String]]
f n|all(==0).concat.q$n=n|1>0=f$foldr(\p b->s b[k,m]p)n$k!m where
 h=length n-1
 t=[0..h]
 e=mapM(\w->[0..w])[h,h]
 i!j=[[x,y]|[x,y]<-e,x>i-2&&x<i+2&&y>j-2&&y<j+2]
 q o=let i#j=o!!i!!j in[[sum[1|[x,y]<-w!l,let
  u=(x-w,y-l)
  p=(w-x,l-y)in u/=p&&('1'==w#l!!(r?u))/=('1'==x#y!!(r?p))]|l<-t]|w<-t]
 v[x,y][a,b]|q n!!x!!y>q n!!a!!b=[x,y]|1>0=[a,b]
 [k,m]=foldr1 v e
 s w[x,y][a,b]|x==a&&y==b=w|'1'==w!!x!!y!!(r?(a-x,b-y))=z '0'|'1'==w!!a!!b!!(r?(x-a,y-b))=z '1'|1>0=w where z u=c w x$c(w!!x)y$c(w!!x!!y)(r?(a-x,b-y))u




{-

r,g,c,d,q,n,f,t,z,s,h,v,

?,#,!,

-}
