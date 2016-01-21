rm(list = ls());
j<-1;
i<-1;
x<-1;
column_sel<- readline("Choose the source by typing the column number: ")
find_route<- read.csv("Sequence_test.csv", header = T);
max_row<- nrow(find_route);
actual_col<- as.numeric(column_sel);
number_row<- actual_col;
actual_col<- rbind(actual_col);
path1<-find_route[column_sel,1];
path_sel<-rbind(path1);
distance_cal<-0;
while(i< (max_row)){
   for(a in seq(1,nrow(find_route))){
    if(path_sel[x,1] == as.numeric(find_route[a,1])){
      number_row<- a;
      break;
    }
  }
  min_distance2<- sort(find_route[,number_row+1])[2]
  for(a in seq(1: nrow(find_route))){
    if(min_distance2 == find_route[a,number_row+1]){
      actual_col_1<-a;
      actual_col<- rbind(actual_col,actual_col_1)
      j<-j+1;
      path3<- find_route[actual_col[j,1],1];
      path_sel<- rbind(path_sel,path3);
      distance_cal<- rbind(distance_cal,min_distance2);
      break;
    }
  } 
  find_route<- find_route[-(number_row),-(number_row+1)];
  i<- i+1;
  x<- x+1;
}
find_route_final<- read.csv("Sequence_test.csv", header = T);
final_path<-"Path";
for(k in seq(1,nrow(find_route_final))){
  final_path<-rbind(final_path,find_route_final[path_sel[k,1],1]);
}
print(final_path[,1]);
Total_Distance<-sum(distance_cal);
print("Total Distance:");
print(Total_Distance)