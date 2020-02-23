library("imager")
img=load.image("~/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
imgGray=grayscale(img)
x=channel(imgGray,1)
matrix_img=as.matrix(imgGray)
matrix_img
matrix_img2=matrix(0,nrow(matrix_img),ncol(matrix_img))
for (i in 1:nrow(matrix_img)) {
  for (j in 1:ncol(matrix_img)) {
      matrix_img[i,j]=as.integer(matrix_img[i,j]*255)
     }
}
bitPlaneSlicing=function(nL,Level){
  mask=0
  for (i in nL:1) {
    mask=mask+2**(Level-i)
  }
  for (i in 1:nrow(matrix_img)) {
    for (j in 1:ncol(matrix_img)) {
      matrix_img2[i,j]= bitwAnd((matrix_img[i,j]),mask)    
    }
  }  
  plot(as.cimg(matrix_img2))
  
}
bitPlaneSlicing(4,7)

