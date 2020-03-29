# LOG FUNCTION IS USED JUST TO CREATE FOURIER SPECTRUM IMAGE
# I HAVE NOT TAKEN FFT OF FILTER BECAUSE THE METHOD WE ARE USING TO CALCULATE THE DISTANCE-
# MATRIX ALREADY WORKS IN FREQUENCY DOMAIN

# RESULT FROM HIGHPASS FILTERING DOESN'T MATCH WITH THE BOOK IMAGES BECAUSE R -
# DOES SOME RESCALING FOR NEGATIVE VALUES AND BECAUSE OF THAT GREYISH MASK IMAGES COME

library(imager)
img.pic = load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/rose.jpeg")
dim(img.pic)
size = rep(128,2)
m = size[1]
n = size[2]
P = m*2
Q = n*2
img.resized = as.matrix(resize(grayscale(img.pic),m,n),m,n)

plot(as.cimg(img.resized))
img.pad = matrix(0,m*2,n*2)

# ----PHASE SHIFTING IN PADDED IMAGE----

for (i in 1:m)
  for (j in 1:n)
  {
    img.pad[i,j] = img.resized[i,j]
    img.pad[i,j] = img.pad[i,j]*((-1)^(i+j))
  }

# ----PHASE SHIFTING IN PADDED IMAGE----


img.fft=fft(img.pad)

range(Re(img.fft))
# range(img.ifft)

test=fft(img.fft,inverse = TRUE)

plot(as.cimg(Re(test)))

# ----LOG IMAGE----

img.log = log(1+abs(img.fft))
plot(as.cimg(img.log))

# ----LOG IMAGE----

D0=50
#----FILTER CREATION----

kernel.mat = matrix(0,P,Q)
for (i in 1:P) {
  for (j in 1:Q) {
    distance=((i-m)^2+(j-n)^2)^(0.5)
    
    # if( distance <= D0){                      #IDEAL PASS FILTER
    #   kernel.mat[i,j] = 0
    # }else{
    #   kernel.mat[i,j] = 1
    # }
    N=2
    # kernel.mat[i,j]=(1/(1 + (D0/distance)^2*N))      #BUTTERWORTH FILTER
    
    # kernel.mat[i,j]=1-exp((-(distance)^2)/(2*(D0)^2)) #GAUSSIAN FILTER
    kernel.mat[i,j] = -4*(pi^2)*distance^2       #LAPLACIAN FILTER
    }
}

#----FILTER CREATION----

img.filtered = kernel.mat*img.fft
img.ifft = fft(img.filtered,inverse = TRUE)/length(img.filtered)
img.ifft = Re(img.ifft)

for (i in 1:P) {
  for (j in 1:Q) {
    img.ifft[i,j] = (img.ifft[i,j])*((-1)^(i+j))
  }
}

img.nopad=matrix(0,m,n)

for (i in 1:m) {
  for (j in 1:n) {
    img.nopad[i,j] = img.ifft[i,j]
  }
}

img.ifftlog=log(1+abs(img.filtered))

plot(as.cimg(Re(img.ifftlog)))
plot(as.cimg(Re(img.nopad)))


# ----CREATED BY PRIYANSHU AGARWAL----
