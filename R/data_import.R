### DATA IMPORT ################################################################

## Load libraries and helper functions

source("R/helper_functions.R")


## Import and clean up station data

station_list <-
  st_read("data/station_list.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  st_as_sf() %>% 
  select(-WKT) %>% 
  st_set_crs(26918) %>% 
  mutate(ID = as.numeric(ID), Year = as.numeric(Year))


## Import subway data

subway <-
  st_read("data", "stops_nyc_subway_nov2018") %>%  
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
  st_transform(26918) %>% 
  st_union()

rm(mh_water, bk_water, qn_water, bx_water, si_water)


## Import, clean up and spread census data

CTs <- get_acs(
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
  geometry = TRUE) %>% 
  as_tibble() %>%
  st_as_sf() %>% 
  st_transform(26918)

names(CTs) <- c("GEOID", "NAME", "Variable", "Estimate", "MOE", "pop_total",
                 "pop_total_MOE", "geometry")

CTs <-
  CTs %>%
  select(-MOE, -pop_total_MOE) %>% 
  spread(key = Variable, value = Estimate) %>% 
  mutate(pop_white = pop_white - pop_hisp_white) %>% 
  select(-pop_hisp_white)


## Clip data to water

CTs <- st_erase(CTs, ny_water)
    

## Get counties and city

counties <- get_acs(
  geography = "county", 
  variables = c(pop_white = "B02001_002"),
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  geometry = TRUE) %>%
  st_transform(26918) %>% 
  st_erase(ny_water) 

city <- 
  st_union(counties)


## Create service and no-service areas for 2018 and 2013

service_2018 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2018) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

no_service_2018 <- 
  CTs %>%
  st_union %>%
  st_erase(service_2018)

service_2013 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2013) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

no_service_2013 <- 
  CTs %>%
  st_union %>%
  st_erase(service_2013)



bike_service_areas <-
  tibble(year = c(2013, 2013, 2018, 2018),
         bike_service = c(TRUE, FALSE, TRUE, FALSE), 
         geometry = c(service_2013, no_service_2013, service_2018, no_service_2018)) %>%
  st_as_sf()

bike_expansion_2013to2018 <- service_2018 %>% 
    st_erase(service_2013)


rm(service_2018, no_service_2018, service_2013, no_service_2013)


## Create subway service and subway no-service areaa

subway_service <- 
  suppressWarnings(subway %>%
                     st_buffer(800) %>%
                     st_union() %>%
                     st_erase(ny_water)) 

subway_no_service <- 
  CTs %>%
  st_union() %>%
  st_erase(subway_service)

geom <- c(subway_service, subway_no_service)

subway_service_areas <-
  tibble(subway_service = c(TRUE, FALSE), 
         geometry = geom) %>% 
  st_as_sf()

rm(subway_service, subway_no_service, geom)

