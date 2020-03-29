library(imager)
photo=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/dakota.jpg")
# photo=resize(photo,500,300)
x=dim(photo)
# photo=boxblur(photo,2)
plot(photo)
zeroMat=array(0,dim = c(x[1],x[2],x[3],1))

redImage=array(c(R(photo),zeroMat,zeroMat),dim=x)
greenImage=array(c(zeroMat,G(photo),zeroMat),dim=x)
blueImage=array(c(zeroMat,zeroMat,B(photo)),dim=x)


# plot(as.cimg(redImage))
# plot(as.cimg(blueImage))
# plot(as.cimg(greenImage))
# photo2=crop.borders(photo,nx=150,ny=100)
# plot(as.cimg(photo2),rescale = FALSE)
# # newArray=array(0,dim = x)
# # newArray=newArray+(photo-photo2)
RC=1
GC=1.4
BC=0.1
# plot(as.cimg((redImage*RC+greenImage*GC+blueImage*BC)))
# plot(as.cimg(photo))
# photo=as.array(photo)
# for (i in 300:x[1]) {
#   for (j in x[2]:400) {
#       photo[i,j,1,1]=photo[i,j,1,1]+1
#   # }
# }
# plot(photo)
# plot(as.cimg((redImage*0.31+greenImage*0.45+blueImage*0.45)))
photo_gray=array(grayscale(photo),dim=x)
plot(crop.borders((photo_gray)+(imshift(redImage*RC,30)+(imshift(blueImage*BC,60))),60,20))
?crop.borders
