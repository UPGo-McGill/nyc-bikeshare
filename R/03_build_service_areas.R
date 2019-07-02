### BUILD SERVICE AREAS ########################################################

## Bike service comparisons for 2013 and 2018

bike_service_comparison <- st_intersect_summarize(
  CTs,
  bike_service_areas,
  group_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))


## Comparison between 2013 access, 2018 new access, and no access

bike_service_growth_comparison <- st_intersect_summarize(
  CTs,
  tibble(
    service = c("service_2013", "service_2018", "no_service"),
    geometry = c(
      filter(bike_service_areas, year == 2013, bike_service == TRUE) %>%
        st_geometry(), 
      st_erase(filter(bike_service_areas, year == 2018, bike_service == TRUE),
               filter(bike_service_areas, year == 2013, bike_service == TRUE)
               ) %>% st_geometry(),
      filter(bike_service_areas, year == 2018, bike_service == FALSE) %>%
        st_geometry())) %>%
      st_as_sf(),
  group_vars = vars(service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))


## Comparison between bike sharing areas with/without transit

bike_service_added_comparison <-
  suppressWarnings(st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  filter(bike_service == TRUE) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(subway_service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education, poverty),
    mean_vars = vars(med_income, vulnerability_index)))


## Comparison between access to both bikeshare/subway and access to neither

subway_service_comparison <- 
  suppressWarnings(st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  st_intersect_summarize(
    CTs,
    ., 
    group_vars = vars(subway_service, bike_service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education, poverty),
    mean_vars = vars(med_income, vulnerability_index)))


## Generate potential expansion service areas

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
  st_erase(bike_service_areas_no_holes$geometry[3]) %>%
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")

subway_buffers <-
  subway_stations %>%
  st_buffer(expansion_distance) %>%
  mutate(bike_proximity = st_distance(subway_stations,
                                      bike_service_areas_no_holes$geometry[3]))

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

target_neighbourhoods <-
  suppressWarnings(subway_stations_vulnerability %>%
                     st_buffer(expansion_distance) %>% 
                     st_intersection(nyc_city) %>% 
                     st_erase(bike_service_areas_no_holes$geometry[3]) %>%
                     st_union() %>%
                     st_cast("POLYGON") %>%
                     as_tibble() %>% 
                     st_as_sf() %>% 
                     mutate(area = st_area(.)) %>%
                     arrange(-area) %>%
                     filter(area > set_units(2000000, m^2)) %>%
                     mutate(nbhd = c("bronx_cluster", "brooklyn_cluster", 
                                     "Jackson Heights/Flushing", 
                                     "Jamaica", "Sunset Park/Bay Ridge", 
                                     "Upper Manhattan", "Far Rockaway")))


clusters <- suppressWarnings(target_neighbourhoods[1:2,] %>%
                               st_intersection(nyc_pumas) %>%   
                               mutate(nbhd = case_when(
                                 PUMACE10 %in% c("03701", "03706", "03801")          ~ "West Bronx",
                                 PUMACE10 %in% c("03705", "03707")                   ~ "Central Bronx",
                                 PUMACE10 %in% c("03702", "03703", "03704", "03709") ~ "East Bronx",
                                 PUMACE10 %in% c("03708", "03710")                   ~ "South Bronx",
                                 PUMACE10 %in% c("04001", "04002", "04003", "04110") ~ "Bushwick/Ridgewood",
                                 PUMACE10 %in% c("04005", "04006", "04007", "04010",
                                                 "04011")                            ~ "Crown Heights/Brownsville",
                                 PUMACE10 %in% c("04008", "04009", "04111", "04113") ~ "East New York/Canarsie")
                               ) %>%
                               st_collection_extract("POLYGON") %>% 
                               filter(!is.na(nbhd))  %>%
                               group_by(nbhd) %>% 
                               summarize())

clusters[7,] <- suppressWarnings(
  clusters[7,] %>%
    st_cast("POLYGON") %>%
    filter(st_area(.) > set_units(10000, m^2)))

target_neighbourhoods <- 
  target_neighbourhoods[3:7,] %>% 
  select(nbhd, geometry) %>% 
  rbind(clusters) %>% 
  arrange(nbhd)


## Get demographics by target neighbourhoods

target_subway_areas <- 
  suppressWarnings(
    st_intersection(target_neighbourhoods,
                    subway_service_areas %>% filter(subway_service == TRUE)))

target_subway_areas <- suppressWarnings(rbind(
  target_subway_areas,
  st_erase(target_neighbourhoods, st_union(target_subway_areas)) %>% 
    mutate(subway_service = FALSE) %>% 
    select(nbhd, subway_service, geometry)))

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
      st_intersection(bike_service_areas_no_holes$geometry[3])) %>% 
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
      st_intersection(bike_service_areas_no_holes$geometry[1])) %>% 
  st_as_sf()


## Analyze demographics

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

