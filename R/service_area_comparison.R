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


## Compare areas with/without transit which got bike sharing

bike_service_added <-
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  filter(bike_service == TRUE)

bike_comparison2018 <- st_intersect_summarize(
  CTs,
  bike_service_added,
  group_vars = vars(subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

# 2. Compare bikeshare access and subway access together vs people with no access to either

transit_access2018 <- 
  bike_service_added <-
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas)

transit_access2018 <- st_intersect_summarize(
  CTs,
  transitaccess2018,
  group_vars = vars(subway_service, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

transit_access2018 <- 
  transitaccess2018 %>% 
  filter((subway_service ==TRUE & bike_service == TRUE) | (subway_service == FALSE & bike_service == FALSE))







