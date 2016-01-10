# Course              : MIS 637
# Final Project       : Location Clusters
# Team Members        : Ricky Bilakhia

setwd("D:")
getwd()
#remove lists
rm(list=ls())

#Required Libraries
library(ggmap)
library(plyr)
#library(sparcl)

#Checking the limit of google map queries
distQueryCheck()

# Read  addresses from practioner data

Address_data <- read.csv(file ="Practitioner_data.csv",header=T,sep = ",");

#checking for Missng values in Practitioner_data

summary(Address_data)
#No missing data is found as the result didn't contain the NA values.

#getting latitude and latitude details for the distinct addresses and writing to csv file
for(distdata in seq(1,nrow((Address_data)))){
  
  Lat_Lon<-geocode(paste(Address_data$Address[distdata],Address_data$City[distdata],Address_data$State[distdata]), 
                   output = c("latlon"));
  write.table(Lat_Lon, file = "Latitude_Longitude.csv", append = T, sep = ",", col.names = F, row.names = F);
}

#Assigning column names 
Col_names <- cbind("Address","City","State","Longitude","Latitude")

#writing Distinct address data to csv file
write.table(Col_names, file = "AddressData_Final.csv", append = T, sep = ",", row.names = F, col.names = F);

#reading the latitude and longitude data
Lat_Lon_data<- read.csv(file="Latitude_Longitude.csv", header = F)

#combine Distinct addresses with latitude and longitude data
Data_Overall<- list(Address_data$Address,Address_data$City,Address_data$State,
                    Lat_Lon_data$V1,Lat_Lon_data$V2)

#writing the combined data
write.table(Data_Overall, file = "AddressData_Final.csv", append = T, sep = ",", row.names = F, col.names = F)

#getting the distinct data from AddressData_Final file.
read_alldata <- read.csv(file = "AddressData_Final.csv",header = TRUE)
alldata_df <- as.data.frame(read_alldata)
distinct_address_data <- ddply(alldata_df,.(alldata_df$Latitude),head,n=1)

#writing the distinct values to file
header_data <- list("Address","City","State","Latitude","Longitude")
write.table(header_data,file="Distinct_AddressData.csv",sep = ",",append = TRUE,row.names = F, col.names = F)
write.table(distinct_address_data[,-1],file = "Distinct_AddressData.csv",append = T,sep = ",",row.names = F,col.names = F)



# Reading the Distinct address data
first_data <- 1;
second_data <- 1;

#Reading the distinct address data
Data_address <- read.csv("Distinct_AddressData.csv",header = T,sep = ",")

#getting row count of distinct address data
rowcount_addressdata <- nrow(Data_address);

#getting the address combinations
from_location <-data.frame(Add= paste(Data_address$Address,Data_address$City, Data_address$State));
to_location <- data.frame(Add= paste(Data_address$Address,Data_address$City, Data_address$State));
lat_location <- data.frame(Data_address$Latitude);
lon_location <- data.frame(Data_address$Longitude);
headerlist_location<-list("Source","Destination","Source_Latitude",
                          "Destination_Latitude","Source_Longitude","Destination_Longigtude");
write.table(headerlist_location,file="Practitioner_Iterateaddress.csv",sep=",",append = TRUE, row.names = F, col.names = F);
for(first_data in seq(first_data:rowcount_addressdata))
{
  while(second_data <= rowcount_addressdata){
    From <- paste(from_location[first_data,1])
    To <- paste(to_location[second_data,1])
    lat_from<-paste(lat_location[first_data,1])
    lat_to<-paste(lat_location[second_data,1])
    lon_from<-paste(lon_location[first_data,1])
    lon_to<-paste(lon_location[second_data,1])
    map <- list(From,To,lat_from,lat_to,lon_from,lon_to)
    write.table(map,file="Practitioner_Iterateaddress.csv",sep=",",append = TRUE, row.names = F,col.names = F);
    second_data <- second_data+1
    
  } 
  first_data <- first_data+1
  second_data <- 1
  
}

#finding the travel time, distance between locations present in the practitioner_combinations file
#reading the csv file
read_route <- read.csv(file = "Practitioner_Iterateaddress.csv", header = T)
rowcount_route <- nrow(read_route)
header_route<-list("source","destination","meter","kilo meter","miles","seconds","minutes","hour")

write.table(header_route,file="Practitioner_Route.csv",sep=",",append = TRUE, row.names = F,col.names = F)
for(routenum in seq(1:rowcount_route)){
  detail_route <- mapdist(as.character(read_route[routenum,1]),as.character(read_route[routenum,2]),mode="driving");
  All_routes<- list(detail_route)
  write.table(All_routes,file="Practitioner_Route.csv",sep=",",append = TRUE, row.names = F,col.names = F)
}

#creating data containing source,destination,zip,lat and lon,time taken, distance

route_distance <- read.csv(file = "Practitioner_Route.csv",header = TRUE)
fulldata_practitioner<- cbind(read_route,route_distance$meter,route_distance$kilo.meter,
                              route_distance$miles,route_distance$seconds,route_distance$minutes,
                              route_distance$hour)
Header_Names_full<-list("Source","Destination","Source_Latitude","Destination_Latitude",
                        "Source_Longitude","Destination_Longigtude",
                        "meter","kilo meter","miles","seconds","minutes","hour");
write.table(Header_Names_full,file="Practitioner_overalldata.csv",sep=",",append = TRUE, row.names = F,col.names = F);
write.table(fulldata_practitioner,file="Practitioner_overalldata.csv",sep=",",append = TRUE, row.names = F,col.names = F)


#Creation of distance matrix

read_finaldata <- read.csv(file="Practitioner_overalldata.csv", header = T)
overall_minutes <- data.frame(read_finaldata$minutes)


mat_minutes <- matrix(nrow = 25,ncol = 25)
colnames(mat_minutes) <- Data_address$Address
row.names(mat_minutes) <- Data_address$Address

m_count <- 1
m_row <- 1
m_min <- 1

for(m_count in seq(m_count:(rowcount_addressdata)))
{
  if(m_count <=rowcount_addressdata){
    while(m_row <= rowcount_addressdata){
      mat_minutes[m_row,m_count]<-overall_minutes[m_min,1]
      m_row <- m_row+1;
      m_min<- m_min+1;
    } 
    m_count <- m_count+1
    m_row<- 1
  }
  else{
    break;
  }
}
write.csv(mat_minutes,file="matrix_distance.csv")

#Finding average time travel between locations

mat_data <-read.csv(file="matrix_distance.csv", header = T)
mat_sortdata <- data.frame(mat_data[,-1])

first_value <- 1
second_value <- 1

while(first_value<=25){
  for (second_value in seq(second_value,25)){
    matrix1<- mat_sortdata[first_value,second_value]
    matrix2<- mat_sortdata[second_value,first_value]
    average<-mean(c(matrix1,matrix2))
    write.table(average,file="Average_TimeData.csv", append = T, row.names = F, col.names = F)
  }
  first_value <- first_value+1;
  second_value <- 1;
}

#creating Average time distance matrix
read_timedata <- read.csv(file="Average_TimeData.csv", header = F)

overall_time <- data.frame(read_timedata)

Avg_mat_minutes <- matrix(nrow = 25,ncol = 25)
colnames(Avg_mat_minutes) <- Data_address$Address
row.names(Avg_mat_minutes) <- Data_address$Address

Avg_m_count <- 1
Avg_m_row <- 1
Avg_m_min <- 1


for(Avg_m_count in seq(Avg_m_count:(rowcount_addressdata)))
{
  if(Avg_m_count <=rowcount_addressdata){
    while(Avg_m_row <= rowcount_addressdata){
      mat_minutes[Avg_m_row,Avg_m_count]<-overall_time[Avg_m_min,1]
      Avg_m_row <- Avg_m_row+1;
      Avg_m_min<- Avg_m_min+1;
    } 
    Avg_m_count <- Avg_m_count+1
    Avg_m_row<- 1
  }
  else{
    break;
  }
}

write.csv(mat_minutes,file="Average_matrix_Time.csv")

# creating 300 rows of distinct avergae time data
read_mat_avg<-read.csv(file="Average_matrix_Time.csv", header = T)
read_mat_avg_df<- data.frame(read_mat_avg[,-1])
mat_row <- 1
mat_col<- 1

while(mat_row<25){
  for (mat_col in seq(mat_col,25)){
    mat1<- read_mat_avg_df[mat_row,mat_col]
    mat2<- read_mat_avg_df[mat_col,mat_row]
    avg<-mean(c(mat1,mat2))
    if(avg == 0){
      
    }
    else{
      write.table(avg,file="Average_distance.csv", append = T, row.names = F, col.names = F)
    }
    
  }
  mat_row<- mat_row+1;
  mat_col<- mat_row;
}

#creating the final distance matrix with time

mat_read_avgtime <-read.csv(file="Average_distance.csv", header =F)
mat_finalavgtime <- matrix(nrow = 25, ncol = 25);

final_m_count <- 1
final_m_row <- 2
final_m_min <- 1

row.names(mat_finalavgtime)<- Data_address$Address
colnames(mat_finalavgtime) <- Data_address$Address

while(final_m_count<25){
  for (final_m_row in seq(final_m_row,25)){
    mat_finalavgtime[final_m_row,final_m_count]<- mat_read_avgtime[final_m_min,1]
    final_m_min <- final_m_min+1
  }
  final_m_count <- final_m_count+1;
  final_m_row <- final_m_count+1;
}
write.table(mat_finalavgtime,file="Final_avg_timetravel.csv", sep=",", col.names = NA )

#Hierarchical clustering
#creating Clusters

source_name<- read.csv(file="Distinct_AddressData.csv", header = T)
avg_distance <- read.csv(file="Final_avg_timetravel.csv")
avg_distance_modified <- data.frame(avg_distance[,-1])
hclust_ouput <- hclust(as.dist(avg_distance_modified ),method = "average")
hclust_cut_tree <- cutree(hclust_ouput,k=2)

#colouring the branches of two clusters in the dendrogram
#library(sparcl)
#ColorDendrogram(hclust_ouput,y = hclust_cut_tree, labels= source_name$Address, main = "location clusters",
 #               branchlength = 10)

hclust_dataframe<- data.frame(hclust_cut_tree)
source_clus<- cbind(source_name,hclust_dataframe)
hclust_Colname<- list("Address","City","State","Longitude","Latitude","ClusterNo")
write.table(source_clus, file="Cluster.csv", sep=",", row.names = F, col.names = hclust_Colname)

#Cluster Separation
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

#Plot the cluster Points
#reading the cluster1 points
Cluster_Read<- read.csv(file="Cluster1_plot.csv", header=T)
Marker_coordinate<- data.frame(def_1 = Cluster_Read$Longitude,def_2 = Cluster_Read$Latitude)
map_cluster <- get_googlemap(c(lon = -74.7770, lat = 40.25),markers = Marker_coordinate, scale = 2, maptype = "roadmap", zoom =11)

cluster1_points <- ggmap(map_cluster, extent = "device") +
  geom_text(data = Cluster_Read,
            aes(x= Cluster_Read$Longitude,
                y = Cluster_Read$Latitude,
                label = Cluster_Read$Address),
            position = position_dodge(width = 2,height = 5),
            color = 'red',
            size = 5,
            hjust = rep(c(1,1,0), length.out = nrow(Cluster_Read)))  


#reading the points of cluster 2
Cluster_Read_2<- read.csv(file="Cluster2_plot.csv", header=T)
Marker_coordinate_2<- data.frame(def_1 = Cluster_Read_2$Longitude,def_2 = Cluster_Read_2$Latitude)
map_cluster_2 <- get_googlemap(c(lon = -74.7770, lat = 40.25),markers = Marker_coordinate_2, scale = 2, maptype = "roadmap", zoom =11)

cluster2_points <- ggmap(map_cluster_2, extent = "device") +
  geom_text(data = Cluster_Read_2,
            aes(x= Cluster_Read_2$Longitude,
                y = Cluster_Read_2$Latitude,
                label = Cluster_Read_2$Address),
            position = position_dodge(width = 2,height = 5),
            color = 'blue',
            size = 5,
            hjust = rep(c(0,1,1.2), length.out = nrow(Cluster_Read_2)))

#Predefined Function multiplot for plotting the two clusters 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#plotting cluster1 and cluster2 together.

multiplot(cluster1_points,cluster2_points,cols = 2)








