### HELPER FUNCTIONS

## Load libraries and fonts

library(osmdata)
library(smoothr)
library(tidycensus)
library(tigris)
library(tmap)
library(tmaptools)
library(units)
library(grid)
library(gridExtra)
library(scales)
library(reticulate)
library(ggpubr)
library(tidyverse)
library(sf)
library(extrafont)

options(tigris_use_cache = TRUE)
suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))


## Set buffer values

bike_distance <- 300
subway_distance <- 800
expansion_distance <- 2000


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
    summarize_at(
      sum_vars,
      ~{sum(. * int_area_pct, na.rm = TRUE) / sum(population_int,
                                                  na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join)) %>% 
    drop_units()
  
}


## index_create helper function

index_create <- function(var) {
  (var - min(var)) / (max(var) - min(var))
}


## service_create helper function

service_create <- function(year, distance) {
  suppressWarnings(bike_stations %>%
                     filter(Year == year) %>%
                     st_buffer(distance) %>%
                     st_union() %>%
                     st_erase(nyc_water))
}


## Map and find demographics for bike and subway access by neighbourhood

network_calculator <- function(bike_path, subway_path, man_erase = FALSE,
                               man_clip = FALSE) {
  
  bike_network <-
    st_read(bike_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union() %>%
    st_erase(nyc_water)
  
  if (man_erase) {
    bike_network <- bike_network %>% 
      st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx")))
  }
  
  if (man_clip) {
    bike_network <- bike_network %>% 
      st_erase(filter(clusters, nbhd == "West Bronx")) %>% 
      st_erase(bronx) 
  }
  
  subway_network <-
    st_read(subway_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union()
  
  bike_catchment <-
    bike_network %>%
    st_polygonize() %>%
    st_buffer(dist = 50)
  
  if (man_clip) {
    bike_catchment <- bike_catchment %>% 
      st_cast("POLYGON") %>% 
      as_tibble() %>% 
      st_as_sf() %>% 
      filter(st_area(.) > set_units(100000, m^2)) %>% 
      st_union()
  }
  
  subway_catchment <-
    subway_network %>%
    st_polygonize() %>%
    st_buffer(dist = 50)
  
  comparison <-
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(bike_catchment,
                        st_erase(bike_catchment, subway_catchment))) %>%
    st_as_sf() %>%
    st_set_crs(26918) %>%
    st_intersect_summarize(
      CTs,
      .,
      group_vars = vars(service),
      population = pop_total,
      sum_vars = vars(pop_white, education, poverty),
      mean_vars = vars(med_income, vulnerability_index)) %>%
    st_drop_geometry()
  
  list(comparison, bike_network, subway_network, bike_catchment)
}


