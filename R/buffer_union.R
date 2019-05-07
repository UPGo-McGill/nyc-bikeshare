## Load packages, census API key, and helper functions

library(tidycensus)
library(tidyverse)
library(sf)
library(units)
library(tigris)
Sys.getenv("CENSUS_API_KEY")

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
# v17 <- load_variables(2017, "acs5", cache = TRUE)


## Import station and subway data

stations <-
  read_csv("data/stations.csv") %>%
  st_as_sf(coords = c("lon","lat")) %>%
  st_set_crs(4326) %>% 
  st_transform(26918)

subway <-
  st_read("data", "nyc_subway") %>%
  st_transform(26918) %>% 
  as_tibble() %>% 
  st_as_sf()


## Import water

mh_water <- area_water("NY", "New York", class = "sf")
bk_water <- area_water("NY", "Kings", class = "sf")
qn_water <- area_water("NY", "Queens", class = "sf")
bx_water <- area_water("NY", "Bronx", class = "sf")
si_water <- area_water("NY", "Richmond", class = "sf")

ny_water <-
  rbind(mh_water, bk_water, qn_water, bx_water, si_water) %>% 
  st_transform(26918)

rm(mh_water, bk_water, qn_water, bx_water, si_water)


## Import, clean up and spread census data

data <- get_acs(
  geography = "tract", 
  variables = c(pop_white = "B02001_002", 
                pop_hisp_white = "B03002_013",
                med_income = "B19013_001",
                immigrant = "B05001_006",
                education = "B16010_041"), 
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  summary_var = "B01003_001",
  geometry = TRUE)

names(data) <- c("GEOID", "NAME", "Variable", "Estimate", "MOE", "pop_total",
                 "pop_total_MOE", "geometry")

data <-
  data %>%
  as_tibble() %>%
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(-MOE, -pop_total_MOE) %>% 
  spread(key = Variable, value = Estimate) %>% 
  mutate(pop_white = pop_white - pop_hisp_white) %>% 
  select(-pop_hisp_white)


## Clip data to water and add CT_area

data <-
  data %>%
  #st_erase(ny_water) %>%
  mutate(CT_area = st_area(.))


## Create buffers of stations and intersect with data

buffer <- st_buffer(stations, 300)
buffer_union <- st_union(buffer)

buffer_intersect <- st_intersection(data, buffer_union) #Why are we doing these two steps? 
buffer_intersect <- st_intersection(data, buffer)

## Mutate new population estimates by intersect polygon

buffer_intersect <- 
  buffer_intersect %>%
  mutate(int_pop_total = pop_total * st_area(.) / CT_area,
         int_pop_white = pop_white * st_area(.) / CT_area,
         int_immigrant = immigrant * st_area(.) / CT_area,
         int_education = education * st_area(.) / CT_area)


## Summarize by variables, union by geometry
# What's the purpose having buffer_union and buffer_intersect?

buffer_union <- 
  buffer_intersect %>% 
  summarize(pop_total  = sum(int_pop_total),
            pop_white  = sum(int_pop_white),
            med_income = sum(med_income * int_pop_total) / pop_total,
            immigrant  = sum(int_immigrant),
            education  = sum(int_education),
            geometry   = st_union(geometry))

## Create acess and noaccess areas, then rbind() into one df

access <- buffer_intersect
access <- buffer_union

noaccess <- 
  data %>% 
  st_erase(buffer_union)

rbind(access, noaccess)

## Recalculate bounding box

bboxes <- sapply(buffer$geometry, st_bbox)
bbox <- c(apply(bboxes[1:2,], 1, min), apply(bboxes[3:4,], 1, max))
attr(bbox, "class") <- "bbox"
attr(st_geometry(buffer), "bbox") <- bbox
rm(bboxes, bbox) # Cleanup


## Create station and subway variables

buffer_union <- 
  buffer_union %>% 
  mutate(
    white_pct = pop_white / pop_total,
    immigrant_pct = immigrant / pop_total,
    education_pct = education / pop_total,
    station_list = st_contains(buffer_union, stations), 
    station_count = map_int(station_list, length),
    subway_list = st_contains(buffer_union, subway), 
    subway_count = map_int(subway_list, length)
  )

## Plot the results

buffer[c("station_count", "white_pct")] %>% plot()
st_union(buffer) %>% plot()


## Build the model

lm(station_count ~ pop_total + white_pct + med_income + immigrant_pct +
     education_pct + subway_count, buffer) %>% 
  summary()

