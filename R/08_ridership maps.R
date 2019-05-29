#### Ridership Maps


### STEP 1. Table rides per station, by season

rider_201806 <-
  read_csv("data/201806-citibike-tripdata.csv") %>% 
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_jun = n())

rider_201812 <-
  read_csv("data/201812-citibike-tripdata.csv") %>%
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_dec = n()) %>% 
  mutate(ID = as.numeric(ID))

rider_2018 <- full_join(rider_201806, rider_201812)
rider_2018[is.na(rider_2018)] <- 0
rider_2018 <- 
  rider_2018 %>% 
  mutate(rides = rides_jun + rides_dec) %>% 
  select(-rides_jun, -rides_dec)

stations_2018 <- 
  filter(bike_stations, Year == 2018) %>% 
  select(-Year) %>% 
  left_join(rider_2018, .) %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_as_sf()


### STEP 2. Mapping to find redudancies 

station_buffer <- st_buffer(stations_2018, dist = 30)
station_overlap <- st_intersection(station_buffer)  #this created 10 extra points; there are also 10 rows that 2 intersections

plot(station_overlap)
tm_shape(station_overlap)+
  tm_fill(col = "red")


### STEP 3. Create voronoi polygons

voronoi <-
  tibble(
    ID = stations_2018$ID,
    rides = stations_2018$rides,
    geometry = stations_2018 %>% 
      st_union() %>% 
      st_voronoi() %>%
      st_collection_extract() %>% 
      st_erase(nyc_water) %>% 
      st_intersection(bike_service_filled)) %>% 
  st_as_sf()


# Example map

tm_shape(voronoi) +
  tm_fill("rides") +
  tm_borders(col = "white") +
  tm_shape(stations_2018$geometry) +
  tm_dots()



voronoi <- deldir(stations_2018$geometry)

voronoi.polygons(stations_2018, bike_service_filled)

SpatialPointsDataFrame(stations_2018)



