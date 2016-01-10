
Cluster1_Separation <- "cluster1";
Cluster2_Separation<- "cluster2";
cluster_hier<-read.csv(file="Cluster.csv", header = T)
nrow(cluster_hier)
for (Cluster_index in seq(1,nrow(cluster_hier))){
  if(cluster_hier$ClusterNo[Cluster_index] == 1){
    Cluster1_Separation<- rbind(Cluster1_Separation,cluster_hier[Cluster_index,])
  }
  else{
    Cluster2_Separation<- rbind(Cluster2_Separation,cluster_hier[Cluster_index,])
  }
}
Colname_cluster<- list("Address","City","State","Longitude","Latitude","ClusterNo")
write.table(Cluster1_Separation[-1,], file="Cluster1_plot.csv", row.names = F, col.names = Colname_cluster, sep = ",")
write.table( Cluster2_Separation[-1,], file="Cluster2_plot.csv" , row.names = F, col.names = Colname_cluster, sep = ",")
