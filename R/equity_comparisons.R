## Equity Comparisons

bike_service_added <- st_intersection(filter(bike_service_areas, year == 2018),
                                      subway_service_areas) %>% 
  filter(bike_service == TRUE)

bike_comparison2018 <- st_intersect_summarize(
  CTs,
  bike_service_added,
  ID_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)
