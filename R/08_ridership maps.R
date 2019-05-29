Ridership Maps


### STEP 1. Table rides per station, by season

rider_201806 <- rider_201806 %>% select(c(4)) 
names(rider_201806) <- "ID"
rider_201806$ID <-as.numeric(rider_201806$ID)
rider_201806 <- rider_201806 %>% 
                group_by(ID) %>% 
                summarize(rides_jun = n())


rider_201812 <- rider_201812 %>% select(c(4))
names(rider_201812) <- "ID"
rider_201812$ID <-as.numeric(rider_201812$ID)
rider_201812 <- rider_201812 %>% 
                group_by(ID) %>% 
                summarize(rides_dec = n())

rider_2018 <- full_join(rider_201806, rider_201812, by = "ID")
rider_2018[is.na(rider_2018)] <- 0
rider_2018$total <-  rider_2018$rides_jun + rider_2018$rides_dec
names(rider_2018) <- c("ID", "Rides (June)", "Rides (December)", "Rides (Combined)")

# join this table to station list, to long/lat

stations_2018 <- filter(bike_stations, Year == 2018) 
stations_2018 <- left_join(rider_2018, stations_2018, by = "ID") %>% 
  st_as_sf() 

#identify and remove NA rows
which(st_is_empty(stations_2018))
stations_2018 <- stations_2018[-c(596,739,797), ]

### STEP 2. Mapping to find redudancies 

station_buffer <- st_buffer(stations_2018, dist = 30)
station_overlap <- st_intersection(station_buffer)  #this created 10 extra points; there are also 10 rows that 2 intersections

plot(station_overlap)
tm_shape(station_overlap)+
  tm_fill(col = "red")

### STEP 3. Create voronoi polygons


#first try
stations_v <- st_voronoi(st_union(stations_2018), bike_service_filled)

tm_shape(stations_v) +
  tm_borders()

plot(stations_v)


voronoi <- deldir(stations_2018$geometry)

voronoi.polygons(stations_2018, bike_service_filled)

SpatialPointsDataFrame(stations_2018)



