### HELPER FUNCTIONS

## Load libraries and fonts

library(smoothr)
library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)
library(tmap)
library(tmaptools)
library(extrafont)
library(osmdata)
options(tigris_use_cache = TRUE)
suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))

## st_erase helper function

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


## st_intersect_summarize helper function

st_intersect_summarize <- function(data, poly, group_vars, population, sum_vars,
                                   mean_vars) {
  
  pop <- enquo(population)
  
  data <- data %>% 
    mutate(CT_area = st_area(.))
  
  intersects <- suppressWarnings(st_intersection(data, poly)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = !! pop * int_area_pct) %>%
    group_by(!!! group_vars)
  
  population <- intersects %>% 
    summarize(!! pop := sum(population_int, na.rm = TRUE))
  
  sums <- intersects %>%
    summarize_at(sum_vars, ~{sum(. * int_area_pct, na.rm = TRUE) / sum(population_int, na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join))
  
}


## index_create helper function

index_create <- function(var) {
  (var - min(var)) / (max(var) - min(var))
}


## service_create helper function

service_create <- function(year) {
  suppressWarnings(bike_stations %>%
                     filter(Year == year) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(nyc_water))
}
