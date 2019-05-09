### SERVICE AREA COMPARISON ####################################################

## Load libraries and helper functions

source("R/helper_functions.R")

st_intersect_summarize(
  CTs,
  buffer_union,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)