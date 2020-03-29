library(imager)
img.pic=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/test4.jpeg")
dim(img.pic)
size=rep(256,2)
img.resized=as.matrix(resize((img.pic),size[1],size[2]),size[1],size[2])
# img.resized2=as.matrix(resize(grayscale(img.pic),size[1],size[2]),size[1],size[2])
plot(as.cimg(img.resized))
img.padded=matrix(0,nrow(img.resized)*2,ncol(img.resized)*2)
P=nrow(img.padded)
Q=ncol(img.padded)
for (i in 1:nrow(img.resized)) {
  for (j in 1:ncol(img.resized)) {
    img.padded[i,j] = img.resized[i,j]
    temp=i+j
    # img.padded[i,j]=((-1)^temp)*img.padded[i,j]
  }
}
plot(as.cimg(img.padded))
img.fft=fft(img.padded)
img.log=log(1+abs(img.fft))
plot(as.cimg(img.log))

kernel.mat=matrix(0,nrow(img.fft),ncol(img.fft))
distance=kernel.mat
nKernel=nrow(kernel.mat)
mKernel=ncol(kernel.mat)

for (i in 1:nKernel) {
  for (j in 1:mKernel) {
    temp=i+j
    # img.fft[i,j] = (((-1)^temp)*img.fft[i,j])
  }
}
for (i in 1:nKernel) {
  for (j in 1:mKernel) {
    distance[i,j] = sqrt((i-(P/2))^2+(j-(Q/2))^2)
  }
}
cutOff <- 40
for (i in 1:nKernel) {
  for (j in 1:mKernel) {
    # 
    if(distance[i,j]>cutOff){       #IDEAL PASS FILTER
      kernel.mat[i,j]=0
    }else{
      kernel.mat[i,j]=1
    }

    # kernel.mat[i,j]=(1/(1 + (distance[i,j]/cutOff)^4))      #BUTTERWORTH FILTER
    # kernel.mat[i,j]=exp((-(distance[i,j])^2)/(2*(cutOff^2))) #GAUSSIAN FILTER
  }
}

kernel.fft = fft(kernel.mat)

img.filtered = (kernel.fft)*img.fft
for (i in 1:nrow(img.filtered)) {
  for (j in 1:ncol(img.filtered)) {
    img.filtered[i,j]=Re(img.filtered[i,j])*(-1^(i+j))
  }
}
img.ifft = Re(fft((img.filtered), inverse = TRUE))/length(img.filtered)
for (i in 1:nrow(img.ifft) ) {
  for (j in 1:ncol(img.ifft)) {
    img.ifft[i,j]=(img.ifft[i,j])*(-1^(i+j))
  }
}

for (i in 1:(nrow(img.resized))) {
  for (j in 1:(ncol(img.resized))) {
    img.resized[i,j]=img.ifft[i,j]
  }
}
# plot(as.cimg(Re(img.resized)+img.resized2))
img.log=log(1+abs(img.filtered))
# plot(as.cimg(img.log),rescale = FALSE)

plot(as.cimg(Re(img.resized)))
