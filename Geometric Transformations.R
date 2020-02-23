library("imager")
Img=load.image("~/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
Img=resize(Img,200,200)
plot(Img)
#Rotation
rotated=rotate_xy(Img,180,100,100)
plot(rotated)
#Shifting or Translation
shifted=imshift(Img,100,100)
shifted=imshift(Img,100,0)
plot(shifted)
#Zoom
dim(Img)


