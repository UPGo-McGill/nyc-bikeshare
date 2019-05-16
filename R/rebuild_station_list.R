### REBUILD STATION LIST #######################################################

## Untested, but in theory should rebuild the station list from raw data online


## Load libraries and helper functions

source("R/helper_functions.R")


## Import CSVs

temp <- tempfile(fileext = ".zip")

download.file(
  "https://s3.amazonaws.com/tripdata/201306-citibike-tripdata.zip", temp)
rider_201306 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201312-citibike-tripdata.zip", temp)
rider_201312 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201406-citibike-tripdata.zip", temp)
rider_201406 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201412-citibike-tripdata.zip", temp)
rider_201412 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201506-citibike-tripdata.zip", temp)
rider_201506 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201512-citibike-tripdata.zip", temp)
rider_201512 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201606-citibike-tripdata.zip", temp)
rider_201606 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201612-citibike-tripdata.zip", temp)

rider_201612 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201706-citibike-tripdata.zip", temp)
rider_201706 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201712-citibike-tripdata.zip", temp)
rider_201712 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201806-citibike-tripdata.zip", temp)
rider_201806 <- read_csv(temp)

download.file(
  "https://s3.amazonaws.com/tripdata/201812-citibike-tripdata.zip", temp)
rider_201812 <- read_csv(temp)

unlink(temp)
rm(temp)


## station_maker FUNCTION

station_maker <- function(table1, table2, year) {
  
  ids06 <- table1[,c(4,6,7,8,10,11)] 
  names(ids06) <-
    c("start", "startlat", "startlong", "end", "endlat", "endlong")
  
  ids12 <- table2[,c(4,6,7,8,10,11)]
  names(ids12) <- 
    c("start", "startlat", "startlong", "end", "endlat", "endlong")
  
  # rbind() 06 and 12 months per year to get stations
  stations <- rbind(ids06, ids12)
  
  # Get all unique start and end ids and compare
  start <- stations[, 1:3]
  end <- stations[, 4:6]
  
  end$end <- as.numeric(end$end)
  end$endlat <- as.numeric(end$endlat)
  end$endlong <- as.numeric(end$endlong)
  
  names(start) <- c("ID", "Latitude", "Longitude")
  names(end) <- c("ID", "Latitude", "Longitude")
  
  stations_total <- rbind(start, end) 
  
  stations_total <- 
    stations_total %>%
    distinct() %>%
    group_by(ID) %>% 
    summarize(Latitude = first(Latitude), Longitude = first(Longitude)) %>% 
    mutate(Year = year)
  
  stations_total
}


## Run function on each year

stations_2013 <- station_maker(rider_201306, rider_201312, 2013)
stations_2014 <- station_maker(rider_201406, rider_201412, 2014)
stations_2015 <- station_maker(rider_201506, rider_201512, 2015)
stations_2016 <- station_maker(rider_201606, rider_201612, 2016)
stations_2017 <- station_maker(rider_201706, rider_201712, 2017)
stations_2018 <- station_maker(rider_201806, rider_201812, 2018)

rm(rider_201306, rider_201312, rider_201406, rider_201412, rider_201506,
   rider_201512, rider_201606, rider_201612, rider_201706, rider_201712,
   rider_201806, rider_201812)


## Bind all dfs together and transform from lat long to geometry

all_stations <- rbind(stations_2013, stations_2014, stations_2015,
                      stations_2016, stations_2017, stations_2018) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), na.fail = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(26918)


## Remove errors

station_list <- 
  station_list %>% 
  slice(st_intersects(
    station_list %>%
      filter(ID == "NULL"), station_list)
    %>% unlist() * -1)

id_3240 <- station_list %>% 
  filter(ID == 3240) %>% 
  slice(2) %>%
  mutate(Year = "2016")

station_list <- 
  station_list %>% 
  filter(!(ID == 3240 & Year == 2016)) %>% 
  filter(ID != 3248) %>%
  rbind(id_3240) %>% 
  arrange(ID, Year)

predicates <- st_intersects(station_list, CTs)
station_list <- station_list[which(lengths(predicates)!=0),]


## Write output to disk

st_write(station_list, "data/station_list.csv",
         layer_options = "GEOMETRY=AS_WKT")
