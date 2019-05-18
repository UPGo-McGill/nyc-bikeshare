### SERVICE AREA COMPARISON ####################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Bike service comparisons for 2013 and 2018

bike_service_comparison <- st_intersect_summarize(
  CTs,
  bike_service_areas,
  group_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education, poverty),
  mean_vars = vars(med_income, vulnerability_index)
)


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
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  filter(bike_service == TRUE) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(subway_service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


## Comparison between access to both bikeshare/subway and access to neither

subway_service_comparison <- 
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  st_intersect_summarize(
    CTs,
    ., 
    group_vars = vars(subway_service, bike_service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education),
    mean_vars = vars(med_income))



### Identify possible locations for future Citibike expansion ####

## Take subway service area, add 2000 m buffers, subtract 800m buffers

expansion_subway_service_areas <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(2000) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,]) )

expansion_bike_service_areas <- 
  suppressWarnings(bike_service_areas[3,] %>% 
                     st_buffer(2000) %>% 
                     st_union() %>% 
                     st_erase(bike_service_areas[3,])) 

expansion_subway_service_areas <-
  expansion_subway_service_areas %>%
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")


## Remove parks from within bike service area          

bike_service_filled <- fill_holes(bike_service_areas$geometry[3], 10000000)


## Take bike service area, add 2000 m buffers, and subtract 300 m buffers

expansion_bike_service_areas <- bike_stations %>%
  filter(Year == 2018) %>%
  st_buffer(2000)%>%
  st_union() %>%
  st_erase(bike_service_filled) 

expansion_bike_service_areas <-
  expansion_bike_service_areas %>%
  st_intersection(nyc_city) %>% 
  st_collection_extract("POLYGON")


## Create 2000 m subway buffers with demographic info for each subway stop

subway_buffers <- subway_stations %>%
  st_buffer(2000) %>%
  # Add column of distance between metro station and 2018 citibike service area
  mutate(
    bike_service_proximity = st_distance(subway_stations, bike_service_filled))

subway_buffer_comparison <- st_intersect_summarize(
  CTs,
  subway_buffers,
  group_vars = vars(stop_name, stop_id, borough),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_comparison <- subway_buffer_comparison %>% 
  mutate(vulnerability_index = as.double(vulnerability_index))

subway_buffer_vulnerability2.75 <-
  subway_buffer_comparison %>%
  filter(vulnerability_index > 2.75)

subway_buffer_vulnerability2.75 <- subway_stations %>% 
  filter(stop_id %in% subway_buffer_vulnerability2.75$stop_id) %>%
  st_buffer(2000) %>% 
  st_intersection(nyc_city) %>% 
  st_erase(bike_service_filled)

subway_buffer_vulnerability2.75 <-
  st_intersection(nyc_pumas, subway_buffer_vulnerability2.75) 

subway_service <- subway_service_areas[1]

target_neighbourhoods <-
  subway_buffer_vulnerability2.75 %>%
  st_union() %>%
  st_collection_extract("POLYGON")

target_neighbourhoods_demographics <- st_intersect_summarize(
  CTs,
  target_neighbourhoods,
  group_vars = vars(NA),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))
st_area(target_neighbourhoods_demographics)
plot(target_neighbourhoods_demographics)


## Create target neighbourhood geography

rockaway <-
  subway_buffer_vulnerability2.75 %>%
  filter(PUMACE10 == "04114") %>%
  st_union()

jamaica <-
  subway_buffer_vulnerability2.75 %>%
  filter(PUMACE10 == "04112" | PUMACE10 == "04111" | PUMACE10 == "04113" |
           PUMACE10 == "04106")

flushing <-
  subway_buffer_vulnerability2.75 %>%
  filter(PUMACE10 == "04103")

sunset_park_bay_bridge <-
  subway_buffer_vulnerability2.75 %>% 
  filter(PUMACE10 == "04012" | PUMACE10 == "04013" |PUMACE10 == "04014")

jackson_heights_corona <- 
  subway_buffer_vulnerability2.75 %>% 
  filter(PUMACE10 == "04112" | PUMACE10 == "04111" | PUMACE10 == "04113" | 
           PUMACE10 == "04106")

jackson_heights_corona <- 
  subway_buffer_vulnerability2.75 %>% 
  filter(PUMACE10 == "04112" | PUMACE10 == "04111" | PUMACE10 == "04113" |
           PUMACE10 == "04106")


## Get demographics by target neighbourhoods

subway_buffer_vulnerability2.75 <- st_intersect_summarize(
  CTs,
  subway_buffer_vulnerability2.75,
  group_vars = vars(PUMACE10, PUMA_name),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_vulnerability2.75 <-
  subway_buffer_vulnerability2.75 %>%
  mutate(vulnerability_index = as.double(vulnerability_index),
          pop_total = as.double(pop_total)) %>%
  filter(pop_total > 1000) 


