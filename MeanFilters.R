library("imager")
Img=load.image("~/Desktop/Work/Digital Image Processing/gitHub/bird.jpg")
Img=resize(Img,300,300)
# Img=channel(resize(Img,400,400),1)
# m=imnoise(as.matrix((Img)),type = "saltnpepper")
# m=imnoise(as.matrix(grayscale(Img)),sd=0.08)
m=as.matrix(grayscale(Img))

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
# m=salt(500)
# m=pepper(500)
# m=imnoise(as.matrix(Img))
plot(as.cimg(m))
# m=as.matrix(grayscale(img))
# plot(as.cimg(m))

m2=matrix(0,nrow(m)+4,ncol(m)+4)
  result=m2
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=m[i,j]
  }
}
#meanFilter
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
#HarmoniMeanFilter
harmonicFilter=function(){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=1/m2[i-x1,j-y1]
          # temp2=filter[k,l]
          SUM=SUM+(temp1)
        }
      }
      result[i,j]=(mFil*nFil)/(SUM)
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
#ContraHarmoniMeanFilter
contraHarmonicFilter=function(Q){
  for (i in 2:(nrow(m2)-1)) {
    for (j in 2:(ncol(m2)-1)) {
      SUM=0.00
      SUM2=0.00
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]**(Q+1)
          # temp1=m2[i-x1,j-y1]
          temp2=m2[i-x1,j-y1]**(Q)
          # temp2=m2[i-x1,j-y1]
          SUM=SUM+(temp1)
          SUM2=SUM+temp2
        }
      }
      result[i,j]=(SUM)/(SUM2)
      # result[i,j]=((SUM)**(Q+1))/((SUM2)**Q)
    }
  }
  # m4=matrix(0,nrow(m),ncol(m))
  # for (i in 1:nrow(m)) {
  #   for (j in 1:ncol(m)) {
  #     m4[i,j]=result[i+2,j+2]
  #   }
  # }
  plot(crop.borders(as.cimg(m2+result),nPix=3))

}
# meanFilter()
geometricFilter()
# harmonicFilter()
# contraHarmonicFilter(1)

