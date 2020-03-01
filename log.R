library("imager")
imgg<-load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/test.jpg")
plot(imgg,rescale=FALSE)

imgrey<-grayscale(imgg)
mat<-as.matrix(imgrey)
mat

matc<-matrix(0,nrow(mat),ncol(mat))
for(i in 1:nrow(mat)){
  for(j in 1:ncol(mat)){
    val<-mat[i,j]
    
    matc[i,j]=log(val)
    
  }
}
cimgg<-as.cimg(matc)
plot(cimgg)