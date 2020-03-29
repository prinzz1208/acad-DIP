library(imager)
img.pic = load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/test4.jpeg")
dim(img.pic)
size = rep(128,2)
m = size[1]
n = size[2]
P = m*2
Q = n*2
img.resized = as.matrix(resize((img.pic),size[1],size[2]),size[1],size[2])

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
# ----PHASE SHIFTING IN FFT----

# for (i in 1:(m*2))
#   for (j in 1:(n*2))
#   {
#     img.fft[i,j]=img.fft[i,j]*((-1)^(i+j))
#   }

# ----PHASE SHIFTING IN FFT----


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
    #   kernel.mat[i,j] = 1
    # }else{
    #   kernel.mat[i,j] = 0
    # }
    
    # kernel.mat[i,j]=(1/(1 + (distance/D0)^4))      #BUTTERWORTH FILTER

    kernel.mat[i,j]=exp((-(distance)^2)/(2*(D0)^2)) #GAUSSIAN FILTER
  }
}

kernel.fft=fft(kernel.mat)

#----FILTER CREATION----
img.filtered = kernel.mat*img.fft

# for (i in 1:P) {
#   for (j in 1:Q) {
#     img.filtered[i,j] = (img.filtered[i,j])*((-1)^(i+j))
#   }
# }

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
    img.nopad[i,j]=img.ifft[i,j]
  }
}

img.ifftlog=log(1+abs(img.filtered))

plot(as.cimg(Re(img.ifftlog)))


plot(as.cimg(Re(img.nopad)))

