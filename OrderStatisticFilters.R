library("imager")
library(SpatialPack)
Img=load.image("~/Desktop/Work/Digital Image Processing/lab/test3.png")
# Img=resize(Img,300,300)
Img=channel(resize(Img,300,300),1)
# m=imnoise(as.matrix((Img)),type = "saltnpepper",var = 0.2)
# m=imnoise(as.matrix(grayscale(Img)),sd=0.1)

# plot(Img)
# m=imnoise(as.matrix(Img))
plot(as.cimg(m))
#meanFilter
filter=matrix(1,3,3)
mFil=nrow(filter)
nFil=ncol(filter)
# m=as.matrix(grayscale(img))
# plot(as.cimg(m))

salt=function(n){
for (k in seq_len(n))
  {
    i=round(runif(1,1,nrow(m)))
    # i=round(runif(1,1,5))
    j=round(runif(1,1,ncol(m)))
    # j=round(runif(1,1,5))
    m[i,j]=1.00;
  }
  return(m)
}
pepper=function(n){
  for (k in seq_len(n))
  {
    i=round(runif(1,1,nrow(m)))
    # i=round(runif(1,1,5))
    j=round(runif(1,1,ncol(m)))
    # j=round(runif(1,1,5))
    m[i,j]=0.00;
  }
  return(m)
}
m=salt(500)
m=pepper(500)
m2=matrix(0,nrow(m)+4,ncol(m)+4)
result=m2
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=m[i,j]
  }
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

#maxFilter
maxFilter=function(){
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
      result[i,j]=max(SUM)
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
#minFilter
minFilter=function(){
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
      result[i,j]=min(SUM)
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
#midpointFilter
midpointFilter=function(){
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
      result[i,j]=0.5*(min(SUM)+max(SUM))
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
#alphaTrimmedFilter
alphaTrimmedFilter=function(d){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=SUM+temp1
        }
      }
      result[i,j]=(1/((mFil*nFil)-d))*SUM
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


medianFilter()
# maxFilter()
# minFilter()
# midpointFilter()
# alphaTrimmedFilter(2)
