### EXPANSION ANALYSIS #########################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Generate potential expansion service areas

expansion_subway_service_areas <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(2000) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,])) %>% 
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

bike_service_filled <- fill_holes(bike_service_areas$geometry[3], 1e+07)

expansion_bike_service_areas <- bike_stations %>%
  filter(Year == 2018) %>%
  st_buffer(2000) %>%
  st_union() %>%
  st_erase(bike_service_filled) %>%
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

subway_buffers <- subway_stations %>%
  st_buffer(2000) %>%
  mutate(bike_proximity = st_distance(subway_stations, bike_service_filled))

subway_buffer_comparison <- st_intersect_summarize(
  CTs,
  subway_buffers,
  group_vars = vars(stop_name, stop_id, borough),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education, poverty),
  mean_vars = vars(med_income, vulnerability_index)) %>% 
  mutate(vulnerability_index = as.double(vulnerability_index))


## Create target neighbourhoods

subway_buffer_vulnerability <-
  subway_buffer_comparison %>%
  filter(vulnerability_index > 2.75)

subway_buffer_vulnerability <-
  subway_stations %>% 
  filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
  st_buffer(2000) %>% 
  st_intersection(nyc_city) %>% 
  st_erase(bike_service_filled) %>% 
  st_intersection(nyc_pumas)

target_neighbourhoods <- 
  subway_buffer_vulnerability %>% 
  st_collection_extract("POLYGON") %>% 
  group_by(PUMACE10) %>% 
  summarize(geometry = st_union(geometry))

target_neighbourhoods <- 
  target_neighbourhoods %>%  
  mutate(nbhd = case_when(
    PUMACE10 %in% c("03701", "03705", "03706", "03707") ~ "West Bronx",
    PUMACE10 %in% c("03702", "03703", "03704", "03709") ~ "East Bronx",
    PUMACE10 %in% c("03708", "03710")                   ~ "South Bronx",
    PUMACE10 %in% c("03801", "03802", "03803", "03804") ~ "North Manhattan",
    PUMACE10 %in% c("04001", "04002", "04003", "04110") ~ "Bushwick/Ridgewood",
    PUMACE10 %in% c("04005", "04006", "04010", "04011") ~ "Crown Heights/East Flatbush",
    PUMACE10 %in% c("04007", "04008", "04009") ~ "East New York/Canarsie",
    PUMACE10 %in% c("04012", "04013", "04014") ~ "Sunset Park/Bay Ridge",
    PUMACE10 %in% c("04101", "04102", "04107", "04108", "04109") ~ "Jackson Heights/Corona",
    PUMACE10 %in% c("04103")                            ~ "Flushing",
    PUMACE10 %in% c("04106", "04111", "04112", "04113") ~ "Jamaica",
    PUMACE10 %in% c("04114")                            ~ "Far Rockaway"
  ))

target_neighbourhoods <- 
  target_neighbourhoods %>% 
  group_by(nbhd) %>% 
  summarize(geometry = st_union(geometry))


## Get demographics by target neighbourhoods

target_neighbourhoods_demographics <- st_intersect_summarize(
  CTs,
  target_neighbourhoods,
  group_vars = vars(NA),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_vulnerability <- st_intersect_summarize(
  CTs,
  subway_buffer_vulnerability,
  group_vars = vars(PUMACE10, PUMA_name),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_vulnerability <-
  subway_buffer_vulnerability %>%
  mutate(vulnerability_index = as.double(vulnerability_index),
         pop_total = as.double(pop_total)) %>%
  filter(pop_total > 1000) 


