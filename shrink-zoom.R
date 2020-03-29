library("imager")
Img=load.image("mario.jpeg")
m=as.matrix(grayscale(resize(Img,300,300)))
m2=matrix(0,nrow(m)*2,ncol(m)*2)
k=0
l=0
plot(as.cimg(m))
zoom=function(){
  for (i in seq(1,nrow(m2),2)) {
    k=k+1
    l=0
    for (j in seq(1,ncol(m2),2)) {
      l=l+1
      m2[i,j]=m[k,l]
    }
  }
  
  plot(as.cimg(m))
  plot(as.cimg(m2))
  m2=meanFilter()
  m2=medianFilter()
  plot(as.cimg(m2))
  
}
shrink=function(x){
  shrinkm=matrix(c(x,0,0,0,x,0,0,0,1),3,3)
  m2=matrix(0,nrow(m),ncol(m))
  for (i in seq(1,nrow(m))) {
    for (j in seq(1,ncol(m))) {
      pointsm=matrix(c(i,j,1),3,1)
      points=shrinkm %*% pointsm
      # print(points)
      m2[points[1],points[2]]=m[i,j]
      }
  }
  # m3=matrix(0,nrow(m)*x,ncol(m)*x)
  # for (i in 1:nrow(m3)) {
  #   for (j in 1:nrow(m3)) {
  #     m3[i,j]=m2[i,j]
  #   }
  # }
  plot(as.cimg(m2))
  # plot(crop.borders(as.cimg(m2),nPix =  ))
  
}
mFil=3
nFil=3
result=m2
meanFilter=function(){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=SUM+(temp1)
        }
      }
      result[i,j]=(SUM)/(mFil*nFil)
    }
  }
  return(result)
  # plot(crop.borders(as.cimg(result),nPix=3))
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
  return(result)
  # plot(crop.borders(as.cimg(result),nPix=3))
}
# zoom()
shrink(0.5)