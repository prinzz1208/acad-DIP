library("imager")
library(spatialfil)
Img=load.image("~/Desktop/Work/Digital Image Processing/gitHub/mario.jpeg")
Img=resize(Img,300,300)
m=as.matrix(grayscale(Img))
m2=matrix(0,nrow(m)+4,ncol(m)+4)
result=m2
filter=matrix(c(0,1,0,1,-4,1,0,1,0),3,3)

for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=m[i,j]
  }
}
#meanFilter
bilateralFilter=function(Sd){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          Gx=(1/((2*pi*(Sd**2))**0.5))*exp((-(temp1**2)/(2*(Sd**2))))
  
          SUM=SUM+Gx
        }
      }
      result[i,j]=(SUM)
    }
  }
  plot(crop.borders(as.cimg(result),nPix=2))
}
#medianFilter
medianFilter=function(){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=vector()
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=append(SUM,temp1)
        }
      }
      result[i,j]=median(SUM)
    }
  }
  # m4=matrix(0,nrow(m),ncol(m))
  # for (i in 1:nrow(m)) {
  #   for (j in 1:ncol(m)) {
  #     m4[i,j]=result[i+2,j+2]
  #   }
  # }
  # plot(as.cimg(m4))
  plot(crop.borders(as.cimg(result),nPix=3))
}
#geometricMeanFilter
geometricFilter=function(){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=1.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=SUM*(temp1)
        }
      }
      result[i,j]=(SUM)**(1/(mFil*nFil))
    }
  }
  # m4=matrix(0,nrow(m),ncol(m))
  # for (i in 1:nrow(m)) {
  #   for (j in 1:ncol(m)) {
  #     m4[i,j]=result[i+2,j+2]
  #   }
  # }
  plot(crop.borders(as.cimg(result),nPix=3))
}
Lap=function(){
  result=m2
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=(m2[i-x1,j-y1])
          temp2=filter[k,l]
          SUM=SUM+(temp1*temp2)
        }
      }
      if(SUM<0)
        result[i,j]=0
      else if(SUM>1)  
        result[i,j]=1
      else
        result[i,j]=SUM
    }
  }
  # m3=as.matrix(crop.borders(as.cimg(m2),nPix=2))
  return(m2+result);
}
Avg=function(){
  filter=matrix(1/16,3,3)
  result=m2
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=(m2[i-x1,j-y1])
          temp2=filter[k,l]
          SUM=SUM+(temp1*temp2)
        }
      }
      if(SUM<0)
        result[i,j]=0
      else if(SUM>1)  
        result[i,j]=1
      else
        result[i,j]=SUM
    }
  }
  # m3=as.matrix(crop.borders(as.cimg(m2),nPix=2))
  return(result);
}

# m2=Lap()
# geometricFilter()
m2=Avg()
# plot(as.cimg(m2))
# m=as.matrix(grayscale(Img))

# m=(m+applyFilter(m,convKernel(2,"gaussian")))
# m=m+applyFilter(m,convKernel(0.5,"sobel"))
# m=m+applyFilter(m,convKernel(0.58,"sharpen"))

plot(as.cimg(m2))
# m2=matrix(0,nrow(m)+4,ncol(m)+4)
# plot(as.cimg(medianFilter()))
