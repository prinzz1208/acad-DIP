library(imager)
Im=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/rose.jpeg")
Im=resize(Im,512,512)
Fimg=FFT(resize(grayscale(Im),512,512))
# Fimg.inv=FFT(Fimg$real,Fimg$imag,inverse = TRUE)
# plot(Fimg)
# plot(Fimg.inv$real)
# plot(Fimg.inv$imag)
# plot(FFT(Fimg$real,inverse = TRUE))
# plot(Fimg$imag)
# Fmdim=dim(Fm)
Fm=array((complex(Fimg$real,Fimg$imag)),dim = c(Fmdim[1]*2,Fmdim[2]*2,Fmdim[3],Fmdim[4]))
# plot(Fm)
# filter=matrix(0,nrow(Fm),ncol(Fm))
filter=array(0,dim = c(Fmdim[1]*2,Fmdim[2]*2,Fmdim[3],Fmdim[4]))
distance=filter
for (i in 1:(Fmdim[1]*2)) {
  for (j in 1:(Fmdim[2]*2)) {
    for (k in 1:Fmdim[3]) {
      for (l in 1:Fmdim[4]) {
        distance[i,j,k,l]=sqrt((i-(nrow(Fm)/2))^2+(j-(ncol(Fm)/2))^2)
      }
    }
    # distance[i,j]=sqrt((i-(nrow(Fm)/2)^2))+(j-((ncol(Fm)/2)^2)))
  }
}

cutOff <- max(distance) * 0.03
for(x in 1:nrow(Fm)){
  for(y in 1: ncol(Fm)){
    for (i in 1:Fmdim[3]) {
      for (j in 1:Fmdim[4]) {
        filter[x,y,i,j] <-  (1/(1 + (distance[x,y,i,j]/cutOff)^8))
      }
    }
  }
}
# filter=resize(filter,nrow(Fimg$real),ncol(Fimg$real))
filter.kernel=FFT(filter)
# FilterImg=Map('*',filter.kernel,Fimg)
FilterImg=Map('*',filter.kernel,Fimg)
# FilterImg=Map('*',Fm,filter)


Img=FFT(FilterImg$real,FilterImg$imag,inverse = TRUE)
plot(as.cimg(Img$real))
# im <- as.cimg(function(x,y) sin(x/5)i+cos(x/4)*sin(y/2),128,128)
# ff <- FFT(im)
# plot(ff$real,main="Real part of the transform")
# plot(ff$imag,main="Imaginary part of the transform")
# sqrt(ff$real^2+ff$imag^2) %>% plot(main="Power spectrum")
# #Check that we do get our image back
# check <- FFT(ff$real,ff$imag,inverse=TRUE)$real #Should be the same as original
# mean((check-im)^2)

