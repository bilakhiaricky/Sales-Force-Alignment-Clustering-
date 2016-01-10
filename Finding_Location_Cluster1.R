library(ggmap);

d<- r;
rm(list = c("clust1_rbind"))
clust1_rbind<- "Cluster_1 points"
clust1_add_bind <- "Cluster_1 Address"
distinct_data<- read.csv(file="Distinct_Data.csv", header=T)
for ( e in d[-1]){
  clus1_add<-paste(distinct_data[e,1],distinct_data[e,2],distinct_data[e,3])
  clust1<-cbind(distinct_data[e,4],distinct_data[e,5]);
  clust1_rbind<- rbind(clust1_rbind,clust1)
  clust1_add_bind<- rbind(clust1_add_bind,clus1_add)
}

mean_latitude<-mean(as.numeric(clust1_rbind[-1,2]))
mean_longitude<- mean(as.numeric(clust1_rbind[-1,1]))

new_address<-revgeocode(cbind(mean_longitude,mean_latitude), output = "address")

for(a in seq(2,nrow(clust1_add_bind))){
  legs_df <- mapdist(new_address,clust1_add_bind[a],mode="driving");
  All_data<- list(legs_df)
  write.table(All_data,file="cluster1_distance.csv",sep=",",append = TRUE, row.names = F,col.names = F);
}

read_clust1<- read.csv(file="cluster1_distance.csv", header =F);

h<-0
for(k in seq(1:nrow(read_clust1))){
if(min(read_clust1[7]) == read_clust1[k,7])
  {
  h<- k
  }
}

read_cluster1<- as.character(read_clust1[h,2])
add_compare<- data.frame(address_combine = paste(distinct_data$Address,distinct_data$City,distinct_data$State))

m<-0
n<-0
for(n in seq(1,25)){
  if(read_cluster1 == add_compare[n,1])
  {
    m<- n
  }
}
print(m)

