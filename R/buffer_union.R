## Load packages, census API key, and helper functions

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)
Sys.getenv("CENSUS_API_KEY")

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
# v17 <- load_variables(2017, "acs5", cache = TRUE)



## Create buffers of stations and intersect with data


buffer_union <- 
  suppressWarnings(station_list %>%
  filter(Year == 2018) %>%
  st_buffer(300) %>%
  st_union())
  
buffer_intersect <- st_intersection(CTs, buffer_union)

## Mutate new population estimates by intersect polygon

buffer_intersect <- 
  buffer_intersect %>%
  mutate(int_pop_total = pop_total * st_area(.) / CT_area,
         int_pop_white = pop_white * st_area(.) / CT_area,
         int_immigrant = immigrant * st_area(.) / CT_area,
         int_education = education * st_area(.) / CT_area)

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
  CTs %>% 
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


## Create station and subway variables

total_pct2018 <- 
  total_accesss_noaccess %>% 
  mutate(
    white_pct = pop_white / pop_total,
    immigrant_pct = immigrant / pop_total,
    education_pct = education / pop_total,
  )

all_pct <- rbind(total_pct2013, total_pct2014, total_pct2015, total_pct2016, total_pct2017, total_pct2018) %>% 
  mutate(Year = )

## Recalculate bounding box

bboxes <- sapply(buffer$geometry, st_bbox)
bbox <- c(apply(bboxes[1:2,], 1, min), apply(bboxes[3:4,], 1, max))
attr(bbox, "class") <- "bbox"
attr(st_geometry(buffer), "bbox") <- bbox
rm(bboxes, bbox) # Cleanup


## Plot the results

buffer[c("station_count", "white_pct")] %>% plot()
st_union(buffer) %>% plot()


## Build the model

lm(station_count ~ pop_total + white_pct + med_income + immigrant_pct +
     education_pct + subway_count, buffer) %>% 
  summary()

