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


### STEP 2. Find duplicates 

overlaps <-
  stations_2018 %>% 
  st_buffer(30) %>% 
  st_intersection() %>% 
  filter(n.overlaps > 1) %>% 
  pull(origins)

stations_2018 <- 
  stations_2018 %>% 
  filter(!(ID %in% stations_2018[unlist(overlaps),]$ID)) %>% 
  rbind(map(overlaps, ~{
    stations <- stations_2018[.x,]
    tibble(ID = min(stations$ID),
           rides = sum(stations$rides),
           geometry = st_centroid(st_union(stations))) %>% 
      st_as_sf()}) %>% 
      do.call(rbind, .) %>% 
      st_as_sf()) %>% 
  arrange(ID)

rm(overlaps)


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


### STEP 4. Analyze demographics

voronoi_comparison <- st_intersect_summarize(
  CTs,
  voronoi,
  group_vars = vars(ID),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index)) %>% 
  left_join(st_drop_geometry(voronoi))

# Rough regression model

lm(rides ~ pop_total + pop_white + education + poverty + med_income,
   data = voronoi_comparison) %>% 
  summary()

