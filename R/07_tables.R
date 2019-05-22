### TABLES AND CHARTS ##########################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


### 1. INTRODUCTION ####

## Vulnerability index quantile

quantile(CTs$vulnerability_index, seq(0, 1, .333333))



### 2. EQUITY AND BIKE SHARING IN NEW YORK ####

## Table 1. Citi Bike service area expansion, 2013-2018

table_1 <- 
  st_intersect_summarize(
    CTs,
    tibble(
      year = 2013:2018,
      geometry = do.call(c,map(2013:2018, service_create, 300))) %>% 
      st_as_sf(),
    group_vars = vars(year), population = pop_total, sum_vars = vars(pop_white),
    mean_vars = vars(med_income)) %>% 
  mutate(service_area_size = set_units(st_area(.), km^2),
         # Station numbers taken from December operating reports:
         # https://www.citibikenyc.com/system-data/operating-reports
         number_of_stations_at_end_of_year = c(338, 325, 456, 585, 740, 746)
         ) %>% 
  st_drop_geometry() %>% 
  select(year, population_in_service_area = pop_total, 
         number_of_stations_at_end_of_year, service_area_size) %>% 
  mutate(population_in_service_area = round(population_in_service_area, -2),
         service_area_size = round(service_area_size, 1))


### 3. WHO HAS ACCESS TO CITI BIKE, AND WHO DOESN'T?





## Table 2.





## Table X. Expansion neighborhood demographics

table_4 <-
  target_neighbourhoods_demographics %>% 
  st_drop_geometry() %>% 
    mutate_at(c("pop_white", "education", "poverty", "pop_no_subway"), round, 3) %>%
    mutate(pop_total = round(pop_total, -3),
           med_income = round(med_income, -2)) %>% 
    select(neighborhood = nbhd, population = pop_total, med_income,
           pct_in_poverty = poverty, pct_non_hispanic_white = pop_white,
           pct_with_bachelors_degree = education,
           pct_without_subway_access = pop_no_subway)
    