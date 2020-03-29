library(imager)
img.pic = load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/test4.jpeg")
img.fft=fft(img.pic)
img.mat=matrix(img.fft,nrow(img.fft),ncol(img.fft))
Sum=0
for (i in 1:nrow(img.mat)) {
  for (j in 1:ncol(img.mat)) {
    Sum=Sum+img.mat[i,j]
  }
}
avg=Sum/(nrow(img.mat)*ncol(img.mat))
print(avg)
