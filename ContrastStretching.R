library("imager")
img=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/mario.jpeg")
plot(img)
gimg=grayscale(img)
m=as.matrix(grayscale(img))

plot(as.cimg(m))
maxI=max(m)
minI=min(m)
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m[i,j]=round(m[i,j]*255.00)
  }
}
levelplot(img)
