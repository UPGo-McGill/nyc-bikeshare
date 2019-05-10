## Equity Comparisons


bike_service_comparison <- 
  bike_service_areas %>% 
  filter(year == 2018)

subway_service_comparison <- 
  bike_service_areas %>% 
  filter(year == 2018)

bike_comparison2018 <- st_intersect_summarize(
  CTs,
  bike_service_comparison,
  ID_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

subway_comparison2018 <- st_intersect_summarize(
  CTs,
  subway_service_comparison,
  ID_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

st_intersection(bike_comparison2018, subway_comparison2018)




                                                                                                       