library(ggmap);
u<- s;
rm(list = c("clust2_rbind"))
clust2_rbind<- "Cluster_2 points"
clust2_add_bind <- "Cluster_2 Address"
distinct_data<- read.csv(file="Distinct_Data.csv", header=T)
for ( aa in u[-1]){
  clus2_add<-paste(distinct_data[aa,1],distinct_data[aa,2],distinct_data[aa,3])
  clust2<-cbind(distinct_data[aa,4],distinct_data[aa,5]);
  clust2_rbind<- rbind(clust2_rbind,clust2)
  clust2_add_bind<- rbind(clust2_add_bind,clus2_add)
}

mean_latitude_2<-mean(as.numeric(clust2_rbind[-1,2]))
mean_longitude_2<- mean(as.numeric(clust2_rbind[-1,1]))

new_address_2<-revgeocode(cbind(mean_longitude_2,mean_latitude_2), output = "address")

for(ab in seq(2,nrow(clust2_add_bind))){
  legs_df_2 <- mapdist(new_address_2,clust2_add_bind[ab],mode="driving");
  All_data_2<- list(legs_df_2)
  write.table(All_data_2,file="cluster2_distance.csv",sep=",",append = TRUE, row.names = F,col.names = F);
}

read_clust2<- read.csv(file="cluster2_distance.csv", header =F);

h2<-0
for(k2 in seq(1,nrow(read_clust2))){
  if(min(read_clust2[7]) == read_clust2[k2,7])
  {
    h2<- k2
  }
}

read_cluster2<- as.character(read_clust2[h2,2])
add_compare<- data.frame(address_combine = paste(distinct_data$Address,distinct_data$City,distinct_data$State))

m2<-0
n2<-0
for(n2 in seq(1,25)){
  if(read_cluster2 == add_compare[n2,1])
  {
    m2<- n2
  }
}

print(m2)