library("imager")
Img=load.image("~/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
Img=resize(Img,300,300)
dim(Img)
dim1=300
dim2=300
dim3=1
dim4=3
redImage=as.matrix(R(Img))
greenImage=as.matrix(G(Img))
blueImage=as.matrix(B(Img))
zeroImage=matrix(0,nrow(redImage),nrow(redImage))
RedArray=array(c(redImage,zeroImage,blueImage*0.25),dim=c(dim1,dim2,dim3,dim4))
GreenArray=array(c(zeroImage,greenImage*0.3,zeroImage),dim=c(dim1,dim2,dim3,dim4))
BlueArray=array(c(zeroImage,zeroImage,blueImage*0.2),dim=c(dim1,dim2,dim3,dim4))
plot(as.cimg(RedArray))
plot(as.cimg(GreenArray))
plot(as.cimg(BlueArray))
Img=RedArray+GreenArray+BlueArray
plot(as.cimg(Img))
filter=matrix(1/16,3,3)
mFil=nrow(filter)
nFil=ncol(filter)
m=as.matrix(grayscale(Img))
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
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j]=result[i+2,j+2]
  }
}
plot(as.cimg(m))
