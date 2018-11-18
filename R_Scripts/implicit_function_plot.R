x<-seq(-4,4,length=1000)
y<-seq(-4,4,length=1000)
# elliptic curve2
z<-outer(x,y,function(x,y) x^3-y^2-x+1 )
contour(x,y,z,levels=0)
par(new=TRUE)
z<-outer(x,y,function(x,y) x^3-y^2-x )
contour(x,y,z,levels=0)
