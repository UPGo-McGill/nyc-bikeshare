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

st_intersect_summarize <- function(data, poly, population, sum_vars,
                                   mean_vars) {
  
  population <- enquo(population)

  intersects <- suppressWarnings(st_intersection(data, poly))
  cols <- length(intersects)
  
  intersects <- 
    intersects %>% 
    mutate(
      population_int = !! population * st_area(.data$geometry) / .data$CT_area
    ) %>%
    mutate_at(sum_vars, list(`int` = ~{
          . * st_area(.data$geometry) / .data$CT_area
          })) %>% 
    mutate_at(mean_vars, list(`int` = ~{
      . * population_int
      }))

  population <- intersects %>% 
    summarize(!! population := sum(!! population, na.rm = TRUE))
  
  sums <- intersects %>%
    st_drop_geometry() %>%
    summarize_at((cols + 1):(cols + length(sum_vars)), sum, na.rm = TRUE)
  
  means <- intersects %>% 
    st_drop_geometry() %>%
    summarize_at(
      (cols + 1 + length(sum_vars)):
        (cols + length(sum_vars) + length(mean_vars)),
      ~{sum(., na.rm = TRUE) / pull(population, 1)})
  
  sums <- st_as_sf(sums, geometry = population$geometry)
  means <- st_as_sf(means, geometry = population$geometry)
  
  reduce(list(population, sums, means), st_join)
}
