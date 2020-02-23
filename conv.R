library("imager")
filter=matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow = TRUE)
filter
rotate <- function(x) t(apply(x, 2, rev))
filter=rotate(rotate(filter))

# img=load.image("~/Desktop/Work/Digital Image Processing/lab/mario.jpeg")
# img=resize(img,100,100)
m=matrix(0,5,5)
m[3,3]=1

mFil=nrow(filter)
nFil=ncol(filter)
# m=as.matrix(grayscale(img))
# plot(as.cimg(m))
m2=matrix(0,nrow(m)+4,ncol(m)+4)
m2
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    m2[i+2,j+2]=m[i,j]
  }
}
result=m2
for (i in 2:(nrow(m2)-1)) {
  for (j in 2:(ncol(m2)-1)) {
    SUM=0
    # for (k in 1:mFil) {
    #   x1=(k-1)-(floor(mFil/2))
    #   for (l in 1:nFil) {
    #     cat("\nx1:",x1," y1:",y1)
    #     y1=(l-1)-(floor(nFil/2))
    # 
    #     temp1=m2[i-x1,j-y1]
    #     temp2=filter[k,l]
    #     SUM=SUM+(temp1*temp2)
    #   }
    # }
    SUM=filter[1,1]*m2[i-1,j-1]+filter[1,2]*m2[i-1,j-0]+filter[1,3]*m2[i-1,j+1]+
      filter[2,1]*m2[i,j-1]+filter[2,2]*m2[i,j]+filter[2,3]*m2[i,j+1]+filter[3,1]*m2[i+1,j-1]+
      filter[3,2]*m2[i+1,j]+filter[3,3]*m2[i+1,j+1]
    result[i,j]=SUM
  }
}
print(result)
# plot(as.cimg(m2))

