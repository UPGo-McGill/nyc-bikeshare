### EXPANSION ANALYSIS #########################################################

## Generate potential expansion service areas

bike_service_filled <- fill_holes(bike_service_areas$geometry[3], 20000)

expansion_subway_service_areas <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(expansion_distance) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,])) %>% 
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

expansion_bike_service_areas <-
  bike_stations %>%
  filter(Year == 2018) %>%
  st_buffer(expansion_distance) %>%
  st_union() %>%
  st_erase(bike_service_filled) %>%
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

subway_buffers <-
  subway_stations %>%
  st_buffer(expansion_distance) %>%
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

target_neighbourhoods <-
  suppressWarnings(subway_stations %>% 
                     filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
                     st_buffer(expansion_distance) %>% 
                     st_intersection(nyc_city) %>% 
                     st_erase(bike_service_filled) %>%
                     st_union() %>%
                     st_cast("POLYGON") %>%
                     tibble() %>% 
                     st_as_sf %>% 
                     mutate(area = st_area(.)) %>%
                     arrange(-area) %>%
                     st_erase(.[8:28,]) %>%
                     mutate(nbhd = c("bronx_cluster", "brooklyn_cluster", "Jackson Heights/Flushing", "Jamaica", "Sunset Park/Bay Ridge", "Upper Manhattan", "Far Rockaway")) )  %>% 
  group_by(nbhd)  %>% 
  summarize(geometry = st_union(.)) 

target_neighbourhoods_clusters <- suppressWarnings(target_neighbourhoods[1:2,] %>%
                      st_intersection(nyc_pumas)%>%   
                      
  mutate(nbhd = case_when(PUMACE10 %in% c("03701", "03706", "03801")          ~ "West Bronx",
                          PUMACE10 %in% c("03705", "03707")                   ~ "Central Bronx",
                          PUMACE10 %in% c("03702", "03703", "03704", "03709") ~ "East Bronx",
                          PUMACE10 %in% c("03708", "03710")                   ~ "South Bronx",
                          PUMACE10 %in% c("04001", "04002", "04003", "04110") ~ "Bushwick/Ridgewood",
                          PUMACE10 %in% c("04005", "04006", "04007", "04010",
                                          "04011")                            ~ "Crown Heights/Brownsville",
                          PUMACE10 %in% c("04008", "04009", "04111", "04113") ~ "East New York/Canarsie"))%>%
    
  st_collection_extract("POLYGON") %>% 
  filter(!is.na(nbhd))  %>%
  group_by(nbhd) %>% 
  summarize(geometry = st_union(geometry)))
    
target_neighbourhoods <- target_neighbourhoods_clusters %>%
  rbind(target_neighbourhoods[3:7,]) %>% 
  arrange(nbhd) %>% 
  mutate(number = 1:12)
  

## Get demographics by target neighbourhoods

target_subway_areas <- 
  suppressWarnings(
    st_intersection(target_neighbourhoods, subway_service_areas[1,]))

target_subway_areas <- suppressWarnings(rbind(
  target_subway_areas,
  st_erase(target_neighbourhoods, st_union(target_subway_areas)) %>% 
    mutate(subway_service = FALSE) %>% 
    select(number, nbhd, subway_service, geometry)))

target_neighbourhoods_demographics <- st_intersect_summarize(
  CTs,
  target_neighbourhoods,
  group_vars = vars(nbhd),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

target_subway_access <- st_intersect_summarize(
  CTs,
  target_subway_areas,
  group_vars = vars(nbhd, subway_service),
  population = pop_total,
  sum_vars = vars(pop_white),
  mean_vars = vars(med_income))

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

