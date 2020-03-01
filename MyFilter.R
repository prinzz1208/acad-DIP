library("imager")
photo=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/test.jpg")
filter=matrix(c(0,-1,0,-1,4,-1,0,-1,0),3,3)

mFil=nrow(filter)
nFil=ncol(filter)
m=as.matrix(grayscale(photo))
plot(as.cimg(m),rescale=FALSE)
m2=matrix(0,nrow(m)+4,ncol(m)+4)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=round((m[i,j]),2)
  }
}
#geometricMeanFilter
geometricFilter=function(){
  result=m2
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=1.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=SUM*(temp1)
        }
      }
      result[i,j]=(SUM)**(1/(mFil*nFil))
    }
  }
  # plot(crop.borders(as.cimg(result),nPix=3))
  return(result);
}
plot(as.cimg(m2),rescale=FALSE)

Lap=function(){
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
          SUM=SUM+(temp1*temp2)
        }
      }
      if(SUM<0)
        result[i,j]=0
      else if(SUM>1)  
        result[i,j]=1
      else
        result[i,j]=SUM
    }
  }
  # m3=as.matrix(crop.borders(as.cimg(m2),nPix=2))
  return(m2+result);
}
Avg=function(){
  filter=matrix(1/4,3,3)
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
          SUM=SUM+(temp1*temp2)
        }
      }
      if(SUM<0)
        result[i,j]=0
      else if(SUM>1)  
        result[i,j]=1
      else
        result[i,j]=SUM
    }
  }
  # m3=as.matrix(crop.borders(as.cimg(m2),nPix=2))
  return(result);
}
# plot(as.cimg(m))
# plot(as.cimg((m+m3)))
# plot(as.cimg(Lap()))
for (i in seq_along(3)) {
  m2=Avg()
  m2=Lap()
}
plot(as.cimg(m2))