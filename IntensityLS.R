library("imager")
img=load.image("~/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
imgGray=grayscale(img)
plot(imgGray)
matrix_img=as.matrix(imgGray)
plot(matrix_img,type="h")
for(i in 1:nrow(matrix_img)){
  for (j in 1:ncol(matrix_img)) {
    if(matrix_img[i,j]>0.15 && matrix_img[i,j]<0.20){
      matrix_img[i,j]=1
    }    
  }
}
plot(matrix_img,type="h")
plot(as.cimg(matrix_img))
