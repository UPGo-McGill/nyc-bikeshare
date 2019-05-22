### EXPANSION ANALYSIS #########################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Generate potential expansion service areas

bike_service_filled <- fill_holes(bike_service_areas$geometry[3], 20000)

expansion_subway_service_areas <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(2000) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,])) %>% 
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

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

subway_stations_vulnerability <-
  subway_stations %>% 
  filter(stop_id %in% subway_buffer_vulnerability$stop_id)

subway_buffer_vulnerability <-
  suppressWarnings(subway_stations %>% 
  filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
  st_buffer(2000) %>% 
  st_intersection(nyc_city) %>% 
  st_erase(bike_service_filled) %>% 
  st_intersection(nyc_pumas))

rm(bike_service_filled)

target_neighbourhoods <- 
  subway_buffer_vulnerability %>% 
  st_collection_extract("POLYGON") %>% 
  group_by(PUMACE10) %>% 
  summarize(geometry = st_union(geometry))

target_neighbourhoods <- 
  target_neighbourhoods %>%  
  mutate(nbhd = case_when(
    PUMACE10 %in% c("03701", "03706")                   ~ "West Bronx",
    PUMACE10 %in% c("03705", "03707")                   ~ "Central Bronx",
    PUMACE10 %in% c("03702", "03703", "03704", "03709") ~ "East Bronx",
    PUMACE10 %in% c("03708", "03710")                   ~ "South Bronx",
    PUMACE10 %in% c("03801", "03802", "03803", "03804") ~ "Upper Manhattan",
    PUMACE10 %in% c("04001", "04002", "04003", "04110") ~ "Bushwick/Ridgewood",
    PUMACE10 %in% c("04005", "04006", "04007", "04010",
                    "04011")                            ~ "Crown Heights/Brownsville",
    PUMACE10 %in% c("04008", "04009", "04111", "04113") ~ "East New York/Canarsie",
    PUMACE10 %in% c("04012", "04013", "04014")          ~ "Sunset Park/Bay Ridge",
    PUMACE10 %in% c("04101", "04102", "04103", "04107",
                    "04108", "04109")                   ~ "Jackson Heights/Flushing",
    PUMACE10 %in% c("04106", "04112")                   ~ "Jamaica",
    PUMACE10 %in% c("04114")                            ~ "Far Rockaway"))

target_neighbourhoods <- 
  target_neighbourhoods %>% 
  filter(!is.na(nbhd)) %>% 
  group_by(nbhd) %>% 
  summarize(geometry = st_union(geometry)) 

## Clean up geometries

split_target <- 
  suppressWarnings(target_neighbourhoods %>% 
                     split(target_neighbourhoods$nbhd) %>% 
                     map(st_cast, "POLYGON"))

# AD HOC CLEAN UP, TO BE REPLACED WITH SYSTEMATIC CLEAN UP
split_target[[5]] <- 
  split_target[[5]] %>% arrange(st_area(.))
split_target[[11]] <- 
  split_target[[11]] %>%
  filter(drop_units(st_area(.)) > 20000)

target_neighbourhoods <- 
  target_neighbourhoods %>% 
  mutate(geometry = c(
    st_geometry(split_target[["Bushwick/Ridgewood"]][4,]),
    st_geometry(split_target[["Central Bronx"]][1,]),
    st_geometry(split_target[["Crown Heights/Brownsville"]][2,]),
    st_geometry(split_target[["East Bronx"]][2,]),
    st_geometry(split_target[["East New York/Canarsie"]][2,]),
    st_geometry(split_target[["Far Rockaway"]][3,]),
    rbind(split_target[["Jackson Heights/Flushing"]][3,2],
          split_target[["Bushwick/Ridgewood"]][1:2,2],
          split_target[["Jamaica"]][1,2]) %>% st_union(),
    rbind(split_target[["Jamaica"]][2,2],
          split_target[["East New York/Canarsie"]][1,2]) %>% st_union(),
    st_geometry(split_target[["South Bronx"]][4,]),
    st_geometry(split_target[["Sunset Park/Bay Ridge"]][2,]),
    st_geometry(split_target[["Upper Manhattan"]][3,]),
    rbind(split_target[["West Bronx"]][1,2],
          split_target[["Upper Manhattan"]][2,2]) %>% st_union()))

rm(split_target)

target_neighbourhoods <- 
  target_neighbourhoods %>% 
  arrange(nbhd) %>% 
  mutate(number = 1:12) %>% 
  select(number, nbhd, geometry)


## Get demographics by target neighbourhoods

target_neighbourhoods_demographics <- st_intersect_summarize(
  CTs,
  target_neighbourhoods,
  group_vars = vars(nbhd),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

target_subway_access <- st_intersect_summarize(
  target_neighbourhoods_demographics,
  subway_service_areas,
  group_vars = vars(nbhd, subway_service),
  population = pop_total,
  sum_vars = vars(pop_white),
  mean_vars = vars(vulnerability_index))

target_neighbourhoods_demographics <- 
  target_neighbourhoods_demographics %>% 
  mutate(pop_no_subway = (target_subway_access %>%
                            filter(subway_service == FALSE) %>%
                            pull(pop_total)) / pop_total) %>% 
  select(everything(), geometry)

target_neighbourhoods <- 
  target_neighbourhoods %>% 
  left_join(st_drop_geometry(
    target_neighbourhoods_demographics[c("nbhd", "vulnerability_index",
                                         "pop_no_subway")]),
    by = "nbhd") %>% 
  mutate(vulnerability_index = as.vector(vulnerability_index))

target_subway_areas <- 
  st_intersection(target_neighbourhoods, subway_service_areas[1,]) %>% 
  select(-vulnerability_index, -pop_no_subway) %>% 
  

target_no_subway_areas <- 
  st_erase(target_neighbourhoods, st_union(target_subway_areas))


