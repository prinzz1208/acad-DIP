  library("imager")
  image1=load.image("/home/prinzz/Desktop/Work/Digital Image Processing/lab/test.jpg")
  m=as.matrix(grayscale(image1))
  L=256
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
       m[i,j]=m[i,j]*255
    }
  }
  xlim=c(0,255)
  tm=tabulate(m)
  n=vector(length = 256)
  for (i in 1:256) {
    if(is.na(tm[i]) || tm[i]==0){
      n[i]=0
    }else{
      n[i]=tm[i]
    }
  }
  
  r=(1:256)
  MN=nrow(m)*ncol(m)
  pr=vector(length = 256)
  for (k in 1:L) {
    if(n[k]!=0){
      temp=(n[k])/MN
      pr[k]=round(temp,2)
    }
  }
  s=vector(length=L)
  for (i in 1:L) {
    if(pr[i]!=0.00){
      Sum=0
      for (k in 1:i) {
        Sum=Sum+pr[k]  
      }
      x=round((L-1)*(Sum))
      if(x>255){
        
        s[i]=255
      }else{
        s[i]=x
      }
    }else{
      s[i]=0
    }
  }
  
  ps=vector(length=L)
  n2=vector(length=L)
  
  
  for (i in 1:L) {
      if(r[i]==0){
        Sum=n[1]
      }else{
        Sum=n[r[i]+1]
      }
      k=i+1
      if(k<256){
        for (j in k:L) {
          if(s[i]==s[j]){
            Sum=Sum+n[r[j]+1]
            # cat("\n",Sum,"In if \n")    
          }
        }
      }
      # cat("\n",Sum," i:",i)
      if(n2[s[i]]==FALSE){
        n2[s[i]]=Sum
      ps[i]=round((Sum/MN),2)
      
    }else{
      ps[i]=0
    }
    # cat("\nn2[s[i]]:",n2[s[i]]," ")
    
  }
  n2
  sum(ps)
  m2=matrix(0,nrow(m),ncol(m))
  
  plot(s,n2,type="h")
  plot(r,n,type="h")
  
  sum(n2)
  sum(n)

  
  
  
  
  
  
  
  
    
  