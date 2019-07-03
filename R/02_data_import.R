### DATA IMPORT ################################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


### Import census geographies ####

## Import water

nyc_water <- suppressMessages(rbind(
  area_water("NY", "New York", class = "sf"),
  area_water("NY", "Kings", class = "sf"),
  area_water("NY", "Queens", class = "sf"),
  area_water("NY", "Bronx", class = "sf"),
  area_water("NY", "Richmond", class = "sf"),
  area_water("NY", "Nassau", class = "sf"),
  area_water("NY", "Westchester", class = "sf"),
  area_water("NJ", "Bergen", class = "sf"),
  area_water("NJ", "Hudson", class = "sf")) %>% 
  st_transform(26918) %>% 
  st_union())


## Import CMA, city and county boundaries

nyc_msa <- suppressMessages(suppressWarnings(
  counties(state = c("New York", "New Jersey"), class = "sf") %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>% 
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond",
                     "Nassau", "Westchester", "Bergen", "Hudson", "Monmouth",
                     "Middlesex", "Somerset", "Morris", "Essex", "Union",
                     "Passaic"),
         !(STATEFP == "36" & NAME == "Essex")) %>% 
  st_erase(
    rbind(
      area_water("NY", "New York", class = "sf"),
      area_water("NY", "Kings", class = "sf"),
      area_water("NY", "Queens", class = "sf"),
      area_water("NY", "Bronx", class = "sf"),
      area_water("NY", "Richmond", class = "sf"),
      area_water("NY", "Nassau", class = "sf"),
      area_water("NY", "Westchester", class = "sf"),
      area_water("NJ", "Bergen", class = "sf"),
      area_water("NJ", "Hudson", class = "sf"),
      area_water("NJ", "Union", class = "sf"),
      area_water("NJ", "Middlesex", class = "sf"),
      area_water("NJ", "Somerset", class = "sf"),
      area_water("NJ", "Morris", class = "sf"),
      area_water("NJ", "Monmouth", class = "sf"),
      area_water("NJ", "Essex", class = "sf"),
      area_water("NJ", "Passaic", class = "sf")) %>% 
      st_transform(26918) %>% 
      st_union()
  )))

nyc_city <- nyc_msa %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>% 
  st_union()

manhattan <- nyc_msa %>% filter(NAME == "New York")
bronx <- nyc_msa %>% filter(NAME == "Bronx")

nyc_msa <- st_union(nyc_msa)


## Import PUMAs

nyc_pumas <- pumas(36, class = "sf") %>% 
  st_transform(26918) %>%
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(PUMA_name = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -INTPTLAT10, -INTPTLON10) %>%
  filter(str_detect(PUMA_name, "NYC-"))


### Import bike and subway data ####

## Import bike data

bike_stations <-
  suppressMessages(suppressWarnings(
    st_read("data/station_list.csv", stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      st_as_sf() %>% 
      select(-WKT) %>% 
      st_set_crs(26918) %>% 
      mutate(ID = as.numeric(ID), Year = as.numeric(Year)) %>% 
      st_erase(bronx)))


## Import subway data

subway_stations <-
  suppressMessages(
    st_read("data", "stops_nyc_subway_nov2018", stringsAsFactors = FALSE) %>%  
      st_transform(26918) %>% 
      as_tibble() %>% 
      st_as_sf() %>%
      mutate(borough = NAMELSAD) %>% 
      select(-NAMELSAD, -stop_id2, -GEOID, -stop_lat, -stop_lon))

subway_lines <-
  opq(bbox = "new york city") %>% 
  add_osm_feature(key = "railway", value = "subway") %>% 
  osmdata_sf() %>%
  `$`("osm_lines") %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  filter(str_detect(name, "Line")) %>% 
  st_union ()



### Import census data ####

## Import, clean up and spread census data

CTs <- suppressMessages(get_acs(
  geography = "tract", 
  variables = c(med_income = "B19013_001",
                poverty = "B17001_002",
                pop_white = "B03002_003",
                immigrant = "B05001_006",
                education = "B16010_041"
                ),
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  summary_var = "B01003_001",
  geometry = TRUE) %>% 
  as_tibble() %>%
  st_as_sf() %>% 
  st_transform(26918) %>% 
  set_names(c("GEOID", "NAME", "Variable", "Estimate", "MOE", "pop_total",
                "pop_total_MOE", "geometry")) %>% 
  select(-MOE, -pop_total_MOE) %>% 
  spread(key = Variable, value = Estimate) %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  filter(pop_total > 100) %>%
  na.omit())


## Create vulnerability index

CTs <- 
  CTs %>%
  mutate(
    std_pov = scale(1 - (poverty/pop_total)),
    std_white = scale(pop_white/pop_total),
    std_ed = scale(education/pop_total),
    std_inc = scale(med_income),
    vulnerability_index = 4 - (index_create(std_pov) + index_create(std_white) +
                                 index_create(std_ed) + index_create(std_inc)),
    vulnerability_index = as.vector(vulnerability_index)
    ) %>% 
  select(-std_pov, -std_white, -std_ed, -std_inc)


# Add percentage variables

CTs <- 
  CTs %>% 
  mutate_at(c("pop_white", "education", "immigrant", "poverty"),
            list(pct = ~. / CTs$pop_total))


## Clip data to water

CTs <- suppressWarnings(st_erase(CTs, nyc_water))



### Create service areas ####

## Create service and no-service areas, with holes filled in for maps

service_years <- do.call(c, map(2013:2018, service_create, bike_distance))

bike_service_areas <-
  tibble(year = c(2013, 2013, 2018, 2018),
         bike_service = c(TRUE, FALSE, TRUE, FALSE), 
         geometry = c(
           service_years[1],
           CTs %>% st_union %>% st_erase(service_years[1]),
           service_years[6],
           CTs %>% st_union %>% st_erase(service_years[6]))) %>%
  st_as_sf()

service_years <- fill_holes(service_years, 20000)

bike_service_areas_no_holes <- 
  tibble(year = c(2013, 2013, 2018, 2018),
         bike_service = c(TRUE, FALSE, TRUE, FALSE), 
         geometry = c(
           service_years[1],
           CTs %>% st_union %>% st_erase(service_years[1]),
           service_years[6],
           CTs %>% st_union %>% st_erase(service_years[6]))) %>%
  st_as_sf()

## Create growth areas

growth_years <- map2(service_years[2:6], service_years[1:5], st_difference)

growth <- tibble(
  year = c("2013", "2014", "2015", "2016", "2017", "2018"),
  geometry = c(service_years[1], st_sfc(growth_years))) %>% 
  st_as_sf()


## Create subway service and subway no-service areaa

subway_service <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(subway_distance) %>%
                     st_union() %>%
                     st_erase(nyc_water)) 

subway_no_service <- 
  CTs %>%
  st_union() %>%
  st_erase(subway_service)

geom <- c(subway_service, subway_no_service)

subway_service_areas <-
  tibble(subway_service = c(TRUE, FALSE), 
         geometry = geom) %>% 
  st_as_sf()


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

temp <- tempfile()

download.file(
  "https://s3.amazonaws.com/tripdata/201306-citibike-tripdata.zip", temp)

rider_201306 <-
  unz(temp, "201306-citibike-tripdata.csv") %>% 
  read_csv() %>% 
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_jun = n())

download.file(
  "https://s3.amazonaws.com/tripdata/201312-citibike-tripdata.zip", temp)

rider_201312 <-
  unz(temp, "2013-12 - Citi Bike trip data.csv") %>% 
  read_csv() %>% 
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_dec = n()) %>% 
  mutate(ID = as.numeric(ID))

download.file(
  "https://s3.amazonaws.com/tripdata/201806-citibike-tripdata.csv.zip", temp)

rider_201806 <-
  unz(temp, "201806-citibike-tripdata.csv") %>% 
  read_csv() %>% 
  select(4) %>% 
  set_names("ID") %>%
  group_by(ID) %>% 
  summarize(rides_jun = n())

download.file(
  "https://s3.amazonaws.com/tripdata/201812-citibike-tripdata.csv.zip", temp)

rider_201812 <-
  unz(temp, "201812-citibike-tripdata.csv") %>% 
  read_csv() %>% 
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

unlink(temp)
rm(rider_201806, rider_201812, rider_2018, rider_201306, rider_201312,
   rider_2013, temp)


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


## Import RData files to avoid rebuilding bike share and subway networks

load("data/osm_networks.RData")
load("data/bike_networks.RData")
load("data/subway_networks.RData")
load("data/networks.RData")


## Clean up

rm(service_years, growth_years, subway_service, subway_no_service, geom)

