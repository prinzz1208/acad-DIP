library("imager")
filter=matrix(1/4,3,3)
img1=load.image("~/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
img1=resize(img1,400,400)
mFil=nrow(filter)
nFil=ncol(filter)
m=as.matrix(grayscale(img1))
plot(as.cimg(m))
m2=matrix(0,nrow(m)+4,ncol(m)+4)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
      m2[i+2,j+2]=m[i,j]
  }
}

result=m2
for (i in 2:(nrow(m2)-1)) {
  for (j in 2:(ncol(m2)-1)) {
    SUM=0.00
    for (k in 1:mFil) {
      x1=(k-1)-(floor(mFil/2))
      for (l in 1:nFil) {
        y1=(l-1)-(floor(nFil/2))
        temp1=m2[i-x1,j-y1]
        temp2=filter[k,l]
        SUM=SUM+(temp1*temp2)
      }
    }
    result[i,j]=SUM
  }
}
dim(result)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j]=result[i+2,j+2]
  }
}
plot(as.cimg(m))
