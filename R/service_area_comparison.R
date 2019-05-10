### SERVICE AREA COMPARISON ####################################################

## Load libraries and helper functions

source("R/helper_functions.R")


## Intersect CTs with service areas

st_intersect_summarize(
  CTs,
  bike_service_areas,
  ID_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)