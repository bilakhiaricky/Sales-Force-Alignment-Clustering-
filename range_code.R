rm(list = ls())
user_input <-  readline("Enter the address : ")
user_latlon <- geocode(user_input,output=c("latlon"))

lat_up <- (user_latlon$lat+2.0)
lat_low <- (user_latlon$lat-2.0)
lon_up <- (user_latlon$lon+2.0)
lon_low <- (user_latlon$lon-2.0)

quake_data <- read.csv(file = "latest_after_cleaning.csv",header = T)
lat_value <- 0
lon_value <- 0
for(i in ( 1:nrow(quake_data))){
if((lat_low < quake_data$lat[i] )&& (quake_data$lat[i] < lat_up)){
  lat <- quake_data$lat[i]
  lat_value <- rbind(lat_value,lat)
  longi <- quake_data$lon[i]
  lon_value <- rbind(lon_value,longi)
}
}

lon_value1 <- 0
for(a in (seq( 2,nrow(lon_value)))){
  if((lon_low < lon_value[i,1] )&& (lon_value[i,1] < lon_up)){
    longi1 <- quake_data$lon[i]
    lon_value1 <- rbind(lon_value1,longi1)
  }
}

for(i in seq(2,nrow(longitude_latitude)) ){
  for(j in seq(2,nrow(lon_value1))){
    if(lon_value1[j] == longitude_latitude[i,1]){
      print("True")
      final_lon <- rbind(cbind(longitude_latitude[i,1],longitude_latitude[i,2]),longi)
    }
  }
}


longitude_latitude <- cbind(lon_value,lat_value)

