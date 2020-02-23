
library("imager")

library("spatialfil")
filter=matrix(c(0,-1,0,-1,4,-1,0,-1,0),3,3)
# rotate <- function(x) t(apply(x, 2, rev))
# filter=rotate(rotate(filter))
# m2=applyFilter(m2,filter)
# filter=convKernel(sigma=1.4,k="laplacian")
# plot(as.cimg(m2))
# filter=as.matrix(filter)
img=load.image("~/Desktop/Work/Digital Image Processing/lab/mario.jpeg")
# img=resize(img,100,100)
mFil=nrow(filter)
nFil=ncol(filter)
m=as.matrix(grayscale(img))
plot(as.cimg(m),rescale=FALSE)
m2=matrix(0,nrow(m)+4,ncol(m)+4)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=round((m[i,j]),2)
  }
}
plot(as.cimg(m2),rescale=FALSE)
m# Lap=function(x,y){
#   p=m2[x,y+1]+m2[x,(y-1)]+m2[x+1,y]+m2[x-1,y]-4*m2[x,y]
#   return(p)
# }
result=m2
for (i in 2:(nrow(m2)-1)) {
  for (j in 2:(ncol(m2)-1)) {
    SUM=0
    for (k in 1:mFil) {
      x1=(k-1)-(floor(mFil/2))
      for (l in 1:nFil) {
        y1=(l-1)-(floor(nFil/2))
        temp1=(m2[i-x1,j-y1])
        temp2=filter[k,l]
        # cat(round(temp1,2))
        SUM=SUM+(temp1*temp2)
      }
    }
    # if(m2[i,j]>=255)
    # {
    #   m2[i,j]=255
    # }else{
      if(SUM<0)
        result[i,j]=0
      else if(SUM>1)  
        result[i,j]=1
      else
        result[i,j]=SUM
    # }
    # m2[i,j]=round(Lap(i,j),2)
    
  }
}
# m3
# plot(as.cimg(m2))
# m3=matrix(0,nrow(m),ncol(m))
# for (i in 1:nrow(m)) {
#   for (j in 1:ncol(m)) {
#     m3[i,j]=m2[i+2,j+2]
#   }
# }
m3=m
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m3[i,j]=result[i+2,j+2]
  }
}
# m3=(0.1)*m3
plot(as.cimg(m))
plot(as.cimg((m+m3)))

