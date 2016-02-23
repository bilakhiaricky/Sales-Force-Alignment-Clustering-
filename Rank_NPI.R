#getwd()
#install.packages('RMySQL');
rm(list = ls())
library(RMySQL);
library(excel.link)

mydb = dbConnect(MySQL(), user='root', password='xxxxx',host='xxxxx', dbname="xxxxx");
top_city<- sprintf("SELECT
	                 distinct(nppes_provider_city) AS 'CITY',
                   count(nppes_provider_city) AS 'PROVIDER_COUNT_CITY',
                   count(distinct(speciality_description)) AS 'COUNT_OF_DISTINCT_SPECIALITY',
                   count(speciality_description) AS 'SPECIALITY_COUNT',
                   count(distinct(npi)) AS 'NPI_COUNT',
                   sum(total_drug_cost) AS 'TOTAL_DRUG_COST',
                   sum(total_claim_count) AS 'TOTAL_CLAIM_COUNT',
                   count(drug_name) AS 'DRUG_COUNT'
                   FROM puf
                   GROUP BY nppes_provider_city
                   ORDER BY sum(total_drug_cost) DESC");
result1 <- dbSendQuery(mydb, top_city)
final_result_top_city <- fetch(result1,n=-1)
dbDisconnect(mydb);

mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare");
top_city<-final_result_top_city$CITY[1];
speciality_sel<- sprintf("select 
	                       distinct(speciality_description),
                         count(speciality_description) AS 'SPECIALITY_COUNT',
                         sum(total_drug_cost) as 'Drug_Cost',
                         count(distinct(npi)) AS 'NPI_COUNT',
                         sum(total_claim_count) AS 'TOTAL_CLAIM COUNT',
                         sum(total_day_supply) AS 'TOTAL_SUPPLY',
                         count(drug_name) AS 'DRUG_COUNT'
                         from 
                         puf 
                         where 
                         nppes_provider_city='%s'
                         group by	
                         speciality_description
                         order by
                         sum(total_drug_cost) desc",top_city);
result2 <- dbSendQuery(mydb, speciality_sel)
final_result_speciality <- fetch(result2,n=-1)
dbDisconnect(mydb);

mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare");
top_speciality<-final_result_speciality$speciality_description[1];
drug_sel<- sprintf("select 
	                 distinct(drug_name) AS 'DRUG_NAME',
                   sum(total_drug_cost) as 'Drug_Cost',
                   count(distinct(npi)) AS 'NPI_COUNT',
                   sum(total_claim_count) AS 'TOTAL_CLAIM_COUNT',
                   sum(total_day_supply) AS 'TOTAL_SUPPLY',
                   count(drug_name) AS 'DRUG_COUNT'
                   from 
                   puf 
                   where 
                   nppes_provider_city='%s' and speciality_description= '%s'
                   group by	
                   drug_name
                   order by
                   sum(total_drug_cost) desc",top_city,top_speciality);
result3 <- dbSendQuery(mydb, drug_sel)
final_result_drug <- fetch(result3,n=-1)
dbDisconnect(mydb);
rank_npi<-1;
for(number_change in seq(1:524)){
mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare");
top_drug<-final_result_drug$DRUG_NAME[number_change];
npi_sel<- sprintf("select 
                  distinct(npi),
                  sum(total_drug_cost) as 'Drug_Cost',
                  sum(total_claim_count) AS 'TOTAL_CLAIM_COUNT',
                  total_day_supply AS 'TOTAL_SUPPLY'
                  from 
                  puf 
                  where 
                  nppes_provider_city= '%s' and speciality_description= '%s' and drug_name= '%s'
                  group by	
                  npi
                  order by
                  sum(total_drug_cost) desc",top_city,top_speciality,top_drug);
result4 <- dbSendQuery(mydb, npi_sel)
final_result_npi <- fetch(result4,n=-1)
dbDisconnect(mydb);

mydb = dbConnect(MySQL(), user='root', password='ricky',host='127.0.0.1', dbname="medicare")
overall_sel<-sprintf("SELECT
	                   COUNT(npi) AS 'PROVIDER',
                     COUNT(DISTINCT(nppes_provider_city)) AS 'COUNT_DISTINCT_CITY',
                     count(distinct(speciality_description)) AS 'COUNT_OF_DISTINCT_SPECIALITY',
                     count(distinct(npi)) AS 'DISTINCT_NPI_COUNT',
                     sum(total_drug_cost) AS 'TOTAL_DRUG_COST',
                     sum(total_claim_count) AS 'TOTAL_CLAIM_COUNT',
                     count(distinct(drug_name)) AS 'DISTINCT_DRUG_COUNT'
                     FROM puf
                     ");
result5 <- dbSendQuery(mydb, overall_sel)
final_result_overall <- fetch(result5,n=-1)
dbDisconnect(mydb);

rank_drug_cost<-(final_result_npi$Drug_Cost[1]/final_result_drug$Drug_Cost[number_change])
rank_claim<-(final_result_npi$TOTAL_CLAIM_COUNT[1]/final_result_drug$TOTAL_CLAIM_COUNT[number_change])
rank_supply<-(final_result_npi$TOTAL_SUPPLY[1]/final_result_drug$TOTAL_SUPPLY[number_change])
rank_drug<-(final_result_drug$DRUG_COUNT[number_change]/final_result_speciality$DRUG_COUNT[1])
rank_speciality<-(final_result_speciality$SPECIALITY_COUNT[1]/final_result_top_city$SPECIALITY_COUNT[1])
rank_city<-(final_result_top_city$PROVIDER_COUNT_CITY[1]/final_result_overall$PROVIDER[1])
rank_npi<- rbind(rank_npi,sum(rank_drug_cost,rank_claim,rank_supply,rank_drug,rank_speciality,rank_city))
}
write.csv(rank_npi,file = 'RANK.csv')