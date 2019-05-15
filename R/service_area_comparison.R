### SERVICE AREA COMPARISON ####################################################

## Load libraries and helper functions

source("R/helper_functions.R")


## Intersect CTs with service areas

bike_service_comparison <- st_intersect_summarize(
  CTs,
  bike_service_areas,
  group_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

subway_service_comparison <- st_intersect_summarize(
  CTs,
  subway_service_areas,
  group_vars = vars(subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)




#compare demographics of original 2013 citibike service area to areas of recent expansion

bike_expansion_2013to2018 <- st_intersect_summarize(
  CTs,
  bike_expansion_2013to2018,
  group_vars = vars("2013 to 2018"),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)


## Compare areas with/without transit which got bike sharing

transit_access2018 <- 
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas)

transit_access2018 <- st_intersect_summarize(
  CTs,
  transit_access2018,
  group_vars = vars(subway_service, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)




##### Identify possible locations for future Citibike expansion #########

# Take subway service area, add 2000m buffers for potential expansion, and subtract 800m buffers

expansion_subway_service_areas <- 
  suppressWarnings(subway %>%
                     st_buffer(2000) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,]) %>% 
                     st_erase(ny_water))


#remove parks from within bike service area          
library(smoothr)
bike_service_filled<- fill_holes(bike_service_areas$geometry[3], 10000000)

# Take citibike service area, add buffers for potential expansion, and subtract 300m buffers

expansion_bike_service_areas <- station_list %>%
                      filter(Year == 2018) %>%
                      st_buffer(2000)%>%
                      st_union() %>%
                      st_erase(bike_service_filled) 


# create 2000m subway buffers with demographic information for each subway stop
subway_buffers <- subway %>%
  st_buffer(2000)  %>%
  mutate(bike_service_proximity = st_distance(subway, bike_service_filled)) # adds column of distance between metro station and the 2018 citibike service area

subway_buffer_comparison <- st_intersect_summarize(
  CTs,
  subway_buffers,
  group_vars = vars(stop_name, bike_service_proximity),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income) )

rm(subway_buffers, subway_stops)

