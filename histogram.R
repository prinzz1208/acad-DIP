library("imager")
library("magick")
image1=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/test.jpg")
m=as.matrix(grayscale(image1))
plot(m,type="h")

for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j]=m[i,j]
  }
}
min1=min(m)
max1=max(m)
r=max1-min1

m3=matrix(0,nrow(m),ncol(m))

for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    f=m[i,j]  
    f=((f-min1)/r)
    m3[i,j]=f
    
  }
}

plot(m,xlab="Intensity",xlim=c(0,1),ylim=c(0,1),type="h")
plot(m3,xlab="Intensity",xlim=c(0,1),ylim=c(0,1),type="h")

plot(as.cimg(m),rescale=FALSE)
plot(as.cimg(m3),rescale = FALSE)
