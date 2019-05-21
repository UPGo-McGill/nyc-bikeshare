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

