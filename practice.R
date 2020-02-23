library("imager")

image1=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/mario.jpeg")
image1
m=as.array((image1))
plot(m)
dim(m)
m2=array(0,dim=c(820,745,1,3))
for (i in 820:1) {
  for (j in 1:745) {
    for (k in 1:3) {
      m2[i,j,1,k]=m[821-i,j,1,k]
    }
  }
}
m=grayscale(image1)
dim(m)
m2=grayscale(as.cimg(m2))
dim(m2)
plot(as.cimg(m))
plot(m2)
m=matrix(m,nrow=820,ncol=745)
m2=matrix(m2,nrow=820,ncol=745)
m[1,1]
dim(m)
dim(m2)
m3=m+m2
m[1,1]
m2[1,1]
m3[1,1]
plot(as.cimg(m))
plot(as.cimg(m3))
