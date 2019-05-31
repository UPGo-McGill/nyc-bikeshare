### RIDERSHIP ANALYSIS #########################################################

## Get Broadway line

broadway <- 
  getbb("manhattan new york city") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

broadway <- 
  rbind(broadway$osm_polygons %>% st_cast("LINESTRING"),
        broadway$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  filter(name == "Broadway") %>%
  st_intersection(manhattan) %>% 
  st_union()


## Table rides per station, by season

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

rider_201306 <-
  read_csv("data/201306-citibike-tripdata.csv") %>% 
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_jun = n())

rider_201312 <-
  read_csv("data/201312-citibike-tripdata.csv") %>%
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_dec = n()) %>% 
  mutate(ID = as.numeric(ID))

rider_2013 <- full_join(rider_201306, rider_201312)
rider_2013[is.na(rider_2013)] <- 0
rider_2013 <- 
  rider_2013 %>% 
  mutate(rides = rides_jun + rides_dec) %>% 
  select(-rides_jun, -rides_dec)

stations_2013 <- 
  filter(bike_stations, Year == 2013) %>% 
  select(-Year) %>% 
  left_join(rider_2013, .) %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_as_sf()


rm(rider_201806, rider_201812, rider_2018, rider_201306, rider_201312,
   rider_2013)


## Find duplicates 

overlaps_2018 <-
  stations_2018 %>% 
  st_buffer(30) %>% 
  st_intersection() %>% 
  filter(n.overlaps > 1) %>% 
  pull(origins)

stations_2018 <- 
  stations_2018 %>% 
  filter(!(ID %in% stations_2018[unlist(overlaps_2018),]$ID)) %>% 
  rbind(map(overlaps_2018, ~{
    stations <- stations_2018[.x,]
    tibble(ID = min(stations$ID),
           rides = sum(stations$rides),
           geometry = st_centroid(st_union(stations))) %>% 
      st_as_sf()}) %>% 
      do.call(rbind, .) %>% 
      st_as_sf()) %>% 
  arrange(ID)

overlaps_2013 <-
  stations_2013 %>% 
  st_buffer(30) %>% 
  st_intersection() %>% 
  filter(n.overlaps > 1) %>% 
  pull(origins)

stations_2013 <- 
  stations_2013 %>% 
  filter(!(ID %in% stations_2013[unlist(overlaps_2013),]$ID)) %>% 
  rbind(map(overlaps_2013, ~{
    stations <- stations_2013[.x,]
    tibble(ID = min(stations$ID),
           rides = sum(stations$rides),
           geometry = st_centroid(st_union(stations))) %>% 
      st_as_sf()}) %>% 
      do.call(rbind, .) %>% 
      st_as_sf()) %>% 
  arrange(ID)

rm(overlaps_2018, overlaps_2013)


## Create voronoi polygons

voronoi_2018 <-
  tibble(
    ID = stations_2018$ID,
    rides = stations_2018$rides / 61,
    geometry = stations_2018 %>% 
      st_union() %>% 
      st_voronoi() %>%
      st_collection_extract() %>% 
      st_erase(nyc_water) %>% 
      st_intersection(bike_service_filled)) %>% 
  st_as_sf()

voronoi_2013 <-
  tibble(
    ID = stations_2013$ID,
    rides = stations_2013$rides / 61,
    geometry = stations_2013 %>% 
      st_union() %>% 
      st_voronoi() %>%
      st_collection_extract() %>% 
      st_erase(nyc_water) %>% 
      st_intersection(bike_service_filled_2013)) %>% 
  st_as_sf()


## Analyze demographics

sum(stations_2018$rides) + sum(stations_2013$rides)
sum(stations_2018$rides) / 61
sum(stations_2013$rides) / 61

mean(stations_2018$rides) / mean(stations_2013$rides)

voronoi_comparison_2018 <-
  st_intersect_summarize(
    CTs,
    voronoi_2018,
    group_vars = vars(ID),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index)) %>% 
  left_join(st_drop_geometry(voronoi_2018)) %>% 
  mutate(ride_density = drop_units(rides / st_area(geometry)),
         pop_density = drop_units(pop_total / st_area(geometry)),
         dist_to_broadway = as.numeric(drop_units(st_distance(geometry,
                                                              broadway))))

voronoi_comparison_2013 <-
  st_intersect_summarize(
    CTs,
    voronoi_2013,
    group_vars = vars(ID),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index)) %>% 
  left_join(st_drop_geometry(voronoi_2013)) %>% 
  mutate(ride_density = drop_units(rides / st_area(geometry)),
         pop_density = drop_units(pop_total / st_area(geometry)),
         dist_to_broadway = as.numeric(drop_units(st_distance(geometry,
                                                              broadway))))


## Correlations and rough regression model

voronoi_comparison_2018 %>% 
  st_drop_geometry() %>% 
  map(~cor(.x, (voronoi_comparison_2018$ride_density)))

regression_2018 <-
  lm(ride_density ~ dist_to_broadway + pop_total + pop_white + education +
     poverty + med_income, data = voronoi_comparison_2018)

summary(regression_2018)

stargazer(regression_2018, type = "text")

voronoi_comparison_2013 %>% 
  st_drop_geometry() %>% 
  map(~cor(.x, (voronoi_comparison_2013$ride_density)))

lm(ride_density ~ dist_to_broadway + pop_total + pop_white + education +
     poverty + med_income, data = voronoi_comparison_2013) %>% 
  summary()

pandoc(regression_html, )
