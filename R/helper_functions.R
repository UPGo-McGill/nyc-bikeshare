## Load libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)


## st_erase helper function

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


## st_intersect_summarize helper function

st_intersect_summarize <- function(x, y, population, sum_vars, mean_vars) {
  
  population <- enquo(population)
  population_int <- paste0(quo_name(population),"_int")
  
  intersects <- suppressWarnings(st_intersection(x, y))
  cols <- length(intersects)
  
  intersects <- 
    intersects %>% 
    mutate(
      !! population_int := !! population * st_area(.data$geometry) /
        .data$CT_area
    ) %>%
    mutate_at(sum_vars, list(`int` =  ~{
          . * st_area(.data$geometry) / .data$CT_area
          }))

  sums <- intersects %>% 
    summarize_at(c((cols):(cols + length(sum_vars) - 1)), sum, na.rm = TRUE)
  
  population_int <- enquo(population_int)
  
  means <- intersects %>% 
    #summarize(sum = sum(!! population))
    summarize_at(mean_vars, list(~{
     sum(., na.rm = TRUE)
      #sum((. * (!! population_int) / (!! population)), na.rm = TRUE)
    }))
  
  st_join(sums, means)
}


st_intersect_summarize(
  CTs,
  buffer_union,
  pop_total,
  vars(pop_white, immigrant, education),
  vars(med_income)
  )


st_intersection(CTs, buffer_union)




## Summarize by variables, union by geometry

access <- 
  buffer_intersect %>% 
  summarize(pop_total  = sum(int_pop_total),
            pop_white  = sum(int_pop_white),
            med_income = sum((med_income * int_pop_total) / pop_total, na.rm = TRUE),
            immigrant  = sum(int_immigrant),
            education  = sum(int_education),
            geometry   = st_union(st_set_precision(geometry, 0)))

## Repeat for noaccess

noaccess <- 
  data %>% 
  st_erase(buffer_union)

noaccess <- 
  noaccess %>%
  mutate(int_pop_total = pop_total * st_area(.) / CT_area,
         int_pop_white = pop_white * st_area(.) / CT_area,
         int_immigrant = immigrant * st_area(.) / CT_area,
         int_education = education * st_area(.) / CT_area)

## Summarize by variables, union by geometry

noaccess <- 
  noaccess %>% 
  summarize(pop_total  = sum(int_pop_total),
            pop_white  = sum(int_pop_white),
            med_income = sum((med_income * int_pop_total) / pop_total, na.rm = TRUE),
            immigrant  = sum(int_immigrant),
            education  = sum(int_education),
            geometry   = st_union(st_set_precision(geometry, 0)))

total_accesss_noaccess <- rbind(access, noaccess)

