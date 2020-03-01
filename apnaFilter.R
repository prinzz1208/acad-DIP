library(imager)
photo=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/rose.jpeg")
x=dim(photo)

zeroMat=array(0,dim = c(x[1],x[2],x[3],1))
redImage=array(c(R(photo),zeroMat,zeroMat),dim=x)
greenImage=array(c(zeroMat,G(photo),zeroMat),dim=x)
blueImage=array(c(zeroMat,zeroMat,B(photo)),dim=x)
# plot(as.cimg(redImage))
# plot(as.cimg(blueImage))
# plot(as.cimg(greenImage))


photo2=crop.borders(photo,nx=150,ny=100)
plot(as.cimg(photo2),rescale = FALSE)
newArray=array(0,dim = x)
newArray=newArray+(photo-photo2)
plot(as.cimg((redImage*0.48+greenImage*0.25+blueImage*0)))