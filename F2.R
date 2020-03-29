library(imager)
img.pic=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/rose.jpeg")
img.pic=resize(img.pic,256,256)
img.dim=dim(img.pic)
img.mat=array(img.pic,dim = img.dim)
img.padded=array(0,dim = c(img.dim[1]*2,img.dim[2]*2,img.dim[3],img.dim[4]))
for (i in 1:img.dim[1]) {
  for (j in 1:img.dim[2]) {
    for (k in 1:img.dim[3]) {
      for (l in 1:img.dim[4]) {
        img.padded[i,j,k,l]=img.mat[i,j,k,l]
        # img.padded[i,j,k,l]=img.padded[i,j,k,l]*((-1)^(i+j))
      }
    }
  }
}

P = nrow(img.padded)
Q = ncol(img.padded)
img.fft=FFT(img.padded)
# img.ifft=FFT(img.fft$real,img.fft$imag,inverse = TRUE)
# plot(img.ifft$real)
kernel.array=array(0,dim = dim(img.padded))
kernel.dim=dim(kernel.array)
kernel.distance=kernel.array
for (i in 1:kernel.dim[1]) {
  for (j in 1:kernel.dim[2]) {
    for (k in 1:kernel.dim[3]) {
      for (l in 1:kernel.dim[4]) {
        # kernel.distance[i,j,k,l]=sqrt((i-(P/2))^2+(j-(Q/2))^2)
        kernel.distance[i,j,k,l]=sqrt((i^2+(j^2)))
      }
    }
  }
}
D0=max(kernel.distance)*0.03
for (i in 1:kernel.dim[1]) {
  for (j in 1:kernel.dim[2]) {
    for (k in 1:kernel.dim[3]) {
      for (l in 1:kernel.dim[4]) {
        kernel.array[i,j,k,l]=exp((-(kernel.distance[i,j,k,l])^2)/(2*(D0^2)))
      
      }
    }
  }
}
kernel.fft=FFT(kernel.array)
img.filtered=Map('*',kernel.fft,img.fft)
# for (i in 1:kernel.dim[1]) {
#   for (j in 1:kernel.dim[2]) {
#     for (k in 1:kernel.dim[3]) {
#       for (l in 1:kernel.dim[4]) {
#         img.filtered$real[i,j,k,l]=img.filtered$real[i,j,k,l]*((-1)^(i+j))
#       }
#     }
#   }
# }
img.ifft=FFT(img.filtered$real,img.filtered$imag,inverse = TRUE)
plot(img.ifft$real)
