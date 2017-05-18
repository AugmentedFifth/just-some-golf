u%w=u!!w
r=zip[-1,0,1,1,1,0,-1,-1][-1,-1,-1,0,1,1,1,0]
g _[]_= -1
g i(e:l)x|x==e=i|1>0=g(i+1)l x
l?x=g 0 l x
d _[]_ _=[]
d i(e:l)t v|i==t=v:l|1>0=e:d(i+1)l t v
c=d 0
f n|all(==0).concat.q$n=n|1>0=f$foldr(\p b->s b[k,m]p)n$k!m where
 h=length n-1
 e=mapM(\w->[0..w])[h,h]
 i!j=[[x,y]|[x,y]<-e,x>i-2&&x<i+2&&y>j-2&&y<j+2]
 q o=[[sum[1|[x,y]<-w!l,let
  u=(x-w,y-l)
  p=(w-x,l-y)in u/=p&&('1'==(o%w%l)%(r?u))/=('1'==(o%x%y)%(r?p))]|l<-[0..h]]|w<-[0..h]]
 v[x,y][a,b]|q n%x%y>q n%a%b=[x,y]|1>0=[a,b]
 [k,m]=foldr1 v e
 s w[x,y][a,b]|x==a&&y==b=w|'1'==w%x%y%(r?(a-x,b-y))=z '0'|'1'==w%a%b%(r?(x-a,y-b))=z '1'|1>0=w where z u=c w x$c(w%x)y$c(w%x%y)(r?(a-x,b-y))u



{---- Test Suite ----}

main :: IO ()
main = print $ f
    [ ["00000100", "00010000", "00000000"]
    , ["00000001", "10000000", "00010000"]
    , ["00000000", "00000000", "10000001"]
    ]

{---- Test Suite ----}
