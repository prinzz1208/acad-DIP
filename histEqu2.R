library("imager")
r=(0:7)
n=vector(length=8)
# for (i in 1:8) {
#   n[i]=readline(prompt="Enter value:")
# }
n=c(790,1023,850,656,329,245,122,81)
n=as.integer(n)
MN=4096
pr=vector(length = 8)
for (k in 1:8) {
  temp=(n[k])/MN
  pr[k]=round(temp,2)
}
s=vector(length=8)
for (i in 1:8) {
  Sum=0
  for (k in 1:i) {
    Sum=Sum+pr[k]  
  }
  s[i]=round(7*(Sum))
}
ps=vector(length=8)
n2=vector(length=8)


for (i in 1:8) {
  Sum=n[r[i]+1]
  k=i+1
  if(k<9){
    temp=s[i]
    for (j in k:8) {
      if(temp==s[j]){
          Sum=Sum+n[r[j]+1]
          # cat("\n",Sum,"In if \n")    
      }
    }
  }
  # cat("\n",Sum," i:",i)
  if(n2[s[i]]==0){
    n2[s[i]]=Sum
  }
  # cat("\nn2[s[i]]:",n2[s[i]]," ")
  
   ps[i]=round((Sum/MN),2)
}
sum(n2)
plot(s,ps,type="h")
sum(n)
