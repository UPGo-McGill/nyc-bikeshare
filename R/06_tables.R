### TABLES AND CHARTS ##########################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Table 1. Citi Bike service area expansion, 2013-2018

table_1 <- 
  st_intersect_summarize(
    CTs,
    tibble(
      year = 2013:2018,
      geometry = c(
        service_create(2013), service_create(2014), service_create(2015),
        service_create(2016), service_create(2017), service_create(2018))) %>% 
      st_as_sf(),
    group_vars = vars(year), population = pop_total, sum_vars = vars(pop_white),
    mean_vars = vars(med_income)) %>% 
  mutate(service_area_size = set_units(st_area(.), km^2),
         number_of_stations_at_end_of_year = c(338, 325, 456, 585, 740, 746)
         ) %>% 
  st_drop_geometry() %>% 
  select(year, population_in_service_area = pop_total, 
         number_of_stations_at_end_of_year, service_area_size) %>% 
  mutate(population_in_service_area = round(population_in_service_area, -3),
         service_area_size = round(service_area_size, 1))
  