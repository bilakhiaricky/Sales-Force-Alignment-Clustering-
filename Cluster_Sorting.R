rm(list = ls())
setwd("C:/Users/Ricky/Documents")
getwd();
mat_read<- read.csv(file="matr_distinct_average.csv", header=T)
#as.numeric(h[2])
#as.numeric(j[5])

cluster1<- (data.frame(mat_read[,24]))
cluster2<- (data.frame(mat_read[,10]))
r<-1
s<-2
a<-1
x<-1
y<-2
for(a in seq(1,25)){
  if (cluster1[a,1] < cluster2[a,1]){
    r<- rbind(r,a)
    x<- rbind(x,cluster1[a,1])
  }
  else{
    s<- rbind(s,a)
    y<- rbind(y,cluster2[a,1])
  }
}

