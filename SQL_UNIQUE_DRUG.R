#getwd()
#install.packages('RMySQL');
rm(list = ls())
a<-0
library(RMySQL);
library(excel.link)
unique_drug_separate1<-'test';
mydb = dbConnect(MySQL(), user='xxxx', password='xxxx',host='xxxxx', dbname="xxxxx");
fetch_row_data<- fetch(dbSendQuery(mydb, "select distinct(speciality_description) from puf"));
unlink("unique_drug.csv")
while (a <= nrow(fetch_row_data)){
  a<- a+1;
  drug_name1<-fetch_row_data[a,1]
  unique_drug<- sprintf("call UNIQUE_DRUG_NAME('%s')",drug_name1)
  rs1 <- dbSendQuery(mydb, unique_drug)
  x1 <- fetch(rs1)
  if(nrow(x1)!= 0){
  unique_drug_separate<-cbind(x1,drug_name1)
  unique_drug_separate1<-rbind(unique_drug_separate1,unique_drug_separate)
  write.table( unique_drug_separate, file="unique_drug.csv" , append = T,row.names = F, col.names =F, sep = ",")
    }
  dbDisconnect(mydb);
  mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare");
# print(a);
#  print(unique_drug_separate);
}
b<- 0;
unique_drug_only<-"UNIQUE DRUG"
unique_drug_list<- read.csv(file="unique_drug.csv", header = F);
while (b <= nrow(unique_drug_list)){
  b<- b+1;
  drug_name_unique<-unique_drug_list[b,1]
fetch_row_data_drug<- sprintf( "select distinct(speciality_description) from puf where drug_name=('%s')",drug_name_unique);
rs3 <- dbSendQuery(mydb, fetch_row_data_drug)
final_result_drug <- fetch(rs3)
unique_drug_only<-rbind(unique_drug_only,final_result_drug)
dbDisconnect(mydb);
mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare");
}
