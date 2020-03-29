#PLEASE TRY TO RUN CODE LINE BY LINE FOR BETTER CLARITY

#read.csv reads 1 row less thus no. of rows in .csv file is m+1

library("imager")
#IMAGE FOR DILATION,EROSION AND BOUNDARY EXTRACTION

Img = load.image("~/Desktop/Work/Digital Image Processing/gitHub/morph.png")
m = as.matrix(round(resize(grayscale(Img),200,200)))
plot(as.cimg(m))

#IMAGE FOR DILATION,EROSION AND BOUNDARY EXTRACTION

# MATRIX FOR HIT OR MISS

m1 = as.matrix(read.csv("/home/prinzz/Desktop/Work/Digital Image Processing/gitHub/Assignment Erosion/hitormiss.csv"),12,12)
View(m,"Original Image")
plot(as.cimg(m))
plot(as.cimg(1-m))

# MATRIX FOR HIT OR MISS

# SE MATRIX FOR EROSION,DILATION,BOUNDARY EXTRACTION
 
# filter.mat = matrix(c(0,0,0,1,1,1,0,1,0),3,3) #DEFAULT MATRIX
# print("Enter SE elements:") #FOR USER INPUT
# for (i in 1:3) {
#   for (j in 1:3) {
#     filter.mat[i,j] = as.integer(readline("Enter SE element:"))
#   }
# }

# SE MATRIX FOR EROSION,DILATION,BOUNDARY EXTRACTION

# SE MATRIX FOR HIT OR MISS

filter.obj = matrix(c(0,1,0,1,1,1,0,1,0),3,3,byrow = TRUE) #DEFAULT MATRIX

# print("Enter object elements:") #FOR USER INPUT
# for (i in 1:3) {
#   for (j in 1:3) {
#     filter.obj[i,j] = as.integer(readline("Enter object element:"))
#   }
# }
filter.objc = 1-filter.obj

# SE MATRIX FOR HIT OR MISS


# FUNCTION FOR PADDING
padding = function(m,mFil,nFil){
  m2 = matrix(0,nrow(m)+(mFil-1)*2,ncol(m)+(nFil-1)*2)
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      m2[i+(mFil-1),j+(nFil-1)]=m[i,j]
    }
  }
  return(m2)
}
# FUNCTION FOR PADDING

erosion=function(m,filter.mat,reverse = FALSE){
  mFil=nrow(filter.mat)
  nFil=ncol(filter.mat)
  if(reverse == TRUE)
    pix = 1
  else
    pix = 0
  m2 = padding(m,mFil,nFil)
  result = m2
  for (i in (mFil-1):(nrow(m2)-(mFil-1))) {
    for (j in (nFil-1):(ncol(m2)-(nFil-1))) {
      flag = TRUE
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          if(k == 2 && l == 2){
            break
          }
          temp1=m2[i-x1,j-y1]
          temp2=filter.mat[k,l]
          if( temp2!=0 && bitwAnd(temp1,temp2) == 0){
            flag = FALSE
            break
          }
        }
      }
      if(flag == FALSE)
        result[i,j] = pix
      else
        result[i,j] = abs(1-pix)
    }
  }
  result2 = crop.borders(as.cimg(result),nx = (mFil-1),ny = (nFil-1))
  return (result2)
}
dilation=function(m,filter.mat,reverse = FALSE){
  mFil=nrow(filter.mat)
  nFil=ncol(filter.mat)
  if(reverse == TRUE)
    pix = 0
  else
    pix = 1
  m2 = padding(m,mFil,nFil)
  result = m2
  
  for (i in (mFil-1):(nrow(m2)-(mFil-1))) {
    for (j in (nFil-1):(ncol(m2)-(nFil-1))) {
      flag = TRUE
      for (k in 1:mFil) {
        x1=(k-1)-(floor(mFil/2))
        for (l in 1:nFil) {
          y1=(l-1)-(floor(nFil/2))
          temp1=m2[i-x1,j-y1]
          temp2=filter.mat[k,l]
          if( temp2 != 0 && bitwAnd(temp1,temp2) == 1){
            flag = FALSE
            break
          }
        }
      }
      if(flag == FALSE)
        result[i,j] = pix
      else
        result[i,j] = abs(1-pix)
    }
  }
  result2 = crop.borders(as.cimg(result),nx = (mFil-1)/2,ny = (nFil-1)/2)
  return(result2)
}
opening = function(m){
  result1 = erosion(m,filter.mat)
  result2 = dilation(as.matrix(result1),filter.mat)
  plot(result2)
}
closing = function(m){
  result1 = dilation(m,filter.mat)
  result2 = erosion(as.matrix(result1),filter.mat)
  plot(result2)
}
hitOrmiss = function(m){
  result1 = as.matrix(erosion(m,filter.obj))
  result2 = as.matrix(erosion(1-m,filter.objc),reverse = TRUE)
  result3 = result2
  View(result1,"Erosion with object")
  View(result2,"Erosion with background of object")
  plot(as.cimg(result1))
  plot(as.cimg(result2))
  
  for (i in 1:nrow(result1)) {      #INTERSECTION
    for (j in 1:ncol(result1)) {
      if(result1[i,j] == 1 && result2[i,j] == 1)
        result3[i,j] = 1
      else
        result3[i,j] = 0
      }
  }
  plot(as.cimg(result3),rescale = FALSE)
  View(result3,"Result Image")
}
boundaryEx = function(m){
  result = m
  m2 =as.matrix(erosion(m,filter.mat))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
        if((m[i,j]-m2[i,j]) <= 0){
          result[i,j] = 0
        }
        else{
          result[i,j] = 1
        }
    }
  }
  plot(as.cimg(result))
}

# plot(erosion(m,filter.mat))
# plot(dilation(m,filter.mat))
# opening(m)
# closing(m)
# boundaryEx(m)
# boundaryEx(as.matrix(erosion(m,filter.mat)))
# boundaryEx(as.matrix(dilation(m,filter.mat)))
# hitOrmiss(m1)
