## Load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)


## st_erase helper function

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


## st_intersect_summarize helper function

st_intersect_summarize <- function(x, y, population, sum_vars, mean_vars,
                                   precision) {
  
  population <- enquo(population)
  
  intersects <- suppressWarnings(st_intersection(x, y))
  
  cols <- length(intersects)
  
  intersects <- 
    intersects %>% 
    mutate_at(sum_vars, list(`int` =  ~{
          . * st_area(.data$geometry) / .data$CT_area
          }))

  intersects %>% 
    summarize_at(sum_vars, list())
  
}


#st_intersect_summarize(CTs, buffer_union, vars(pop_total, pop_white, immigrant, education))
