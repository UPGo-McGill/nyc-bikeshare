## Load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)
library(tmap)
options(tigris_use_cache = TRUE)


## st_erase helper function

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


## st_intersect_summarize helper function

st_intersect_summarize <- function(data, poly, ID_vars, population, sum_vars,
                                   mean_vars) {
  
  population <- enquo(population)
  
  data <- data %>% 
    mutate(CT_area = st_area(.))
  
  intersects <- suppressWarnings(st_intersection(data, poly)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = !! population * int_area_pct) %>%
    group_by(!!! ID_vars)
  
  population <- intersects %>% 
    summarize(!! population := sum(population_int, na.rm = TRUE))
  
  sums <- intersects %>%
    summarize_at(sum_vars, ~{sum(. * int_area_pct, na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  reduce(list(population, st_drop_geometry(sums), st_drop_geometry(means)),
         full_join)
}
