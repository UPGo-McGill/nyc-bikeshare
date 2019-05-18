### DATA IMPORT ################################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")



### Import census geographies ####

## Import water

nyc_water <- rbind(
  area_water("NY", "New York", class = "sf"),
  area_water("NY", "Kings", class = "sf"),
  area_water("NY", "Queens", class = "sf"),
  area_water("NY", "Bronx", class = "sf"),
  area_water("NY", "Richmond", class = "sf"),
  area_water("NY", "Nassau", class = "sf"),
  area_water("NY", "Westchester", class = "sf"),
  area_water("NJ", "Bergen", class = "sf"),
  area_water("NJ", "Hudson", class = "sf"),
  area_water("NJ", "Union", class = "sf"),
  area_water("NJ", "Middlesex", class = "sf"),
  area_water("NJ", "Somerset", class = "sf"),
  area_water("NJ", "Morris", class = "sf"),
  area_water("NJ", "Monmouth", class = "sf"),
  area_water("NJ", "Essex", class = "sf"),
  area_water("NJ", "Passaic", class = "sf")) %>% 
  st_transform(26918) %>% 
  st_union()


## Import CMA, city and county boundaries

nyc_msa <- suppressWarnings(
  counties(state = c("New York", "New Jersey"), class = "sf") %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>% 
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond",
                     "Nassau", "Westchester", "Bergen", "Hudson", "Monmouth",
                     "Middlesex", "Somerset", "Morris", "Essex", "Union",
                     "Passaic"),
         !(STATEFP == "36" & NAME == "Essex")) %>% 
  st_erase(nyc_water))

nyc_city <- nyc_msa %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>% 
  st_union()

bronx <- nyc_msa %>% 
  filter(NAME == "Bronx")

nyc_msa <- st_union(nyc_msa)


## Import PUMAs

nyc_pumas <- pumas(36, class = "sf") %>% 
  st_transform(26918) %>%
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(PUMA_name = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -INTPTLAT10, -INTPTLON10) %>%
  filter(str_detect(PUMA_name, "NYC-"))



### Import bike and subway data ####

## Import bike data

bike_stations <-
  st_read("data/station_list.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  st_as_sf() %>% 
  select(-WKT) %>% 
  st_set_crs(26918) %>% 
  mutate(ID = as.numeric(ID), Year = as.numeric(Year))


## Import subway data

subway_stations <-
  st_read("data", "stops_nyc_subway_nov2018") %>%  
  st_transform(26918) %>% 
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(borough = NAMELSAD) %>% 
  select(-NAMELSAD, -stop_id2, -GEOID, -stop_lat, -stop_lon)

subway_lines <-
  opq(bbox = "new york city") %>% 
  add_osm_feature(key = "railway", value = "subway") %>% 
  osmdata_sf() %>%
  `$`("osm_lines") %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  filter(str_detect(name, "Line"))



### Import census data ####

## Import, clean up and spread census data

CTs <- get_acs(
  geography = "tract", 
  variables = c(pop_white = "B02001_002", 
                pop_hisp_white = "B03002_013",
                med_income = "B19013_001",
                immigrant = "B05001_006",
                education = "B16010_041",
                poverty = "B17001_002"),
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
  select(-pop_hisp_white) %>%
  mutate(pop_density = pop_total/st_area(geometry)) 

CTs <- CTs %>% filter(pop_total > 100) %>% na.omit()


## Create vulnerability index

CTs <- 
  CTs %>%
  mutate(
    std_pov = scale(1 - (poverty/pop_total)),
    std_white = scale(pop_white/pop_total),
    std_ed = scale(education/pop_total),
    std_inc = scale(med_income),
    vulnerability_index = 4 - (index_create(std_pov) + index_create(std_white) +
                                 index_create(std_ed) + index_create(std_inc))
    ) %>% 
  select(-std_pov, -std_white, -std_ed, -std_inc)


# Add additional variables

CTs <- 
  CTs %>% 
  mutate(white_percent = pop_white / pop_total * 100,
         education_percent = education / pop_total * 100,
         immigrant_percent = immigrant / pop_total * 100,
         poverty_percent = poverty / pop_total * 100)


## Clip data to water

CTs <- suppressWarnings(st_erase(CTs, nyc_water))



### Create service areas ####

## Create service and no-service areas

service_2013 <- service_create(2013)
service_2014 <- service_create(2014)
service_2015 <- service_create(2015)
service_2016 <- service_create(2016)
service_2017 <- service_create(2017)
service_2018 <- service_create(2018)

no_service_2018 <- CTs %>% st_union %>% st_erase(service_2018)
no_service_2013 <- CTs %>% st_union %>% st_erase(service_2013)

bike_service_areas <-
  tibble(year = c(2013, 2013, 2018, 2018),
         bike_service = c(TRUE, FALSE, TRUE, FALSE), 
         geometry = 
           c(service_2013, no_service_2013, service_2018, no_service_2018)) %>%
  st_as_sf()

bike_expansion_2013to2018 <- st_erase(service_2018, service_2013)


## Create growth areas

growth_2018 <- st_difference(service_2018, service_2017)
growth_2017 <- st_difference(service_2017, service_2016) %>% st_erase(bronx)
growth_2016 <- st_difference(service_2016, service_2015)
growth_2015 <- st_difference(service_2015, service_2014)
growth_2014 <- st_difference(service_2014, service_2013)

growth <- tibble(
  year = c("2013", "2014", "2015", "2016", "2017", "2018"),
  geometry = c(service_2013, growth_2014,growth_2015, growth_2016, growth_2017,
               growth_2018)) %>% 
  st_as_sf()


## Create subway service and subway no-service areaa

subway_service <- 
  suppressWarnings(subway_stations %>%
                     st_buffer(800) %>%
                     st_union() %>%
                     st_erase(nyc_water)) 

subway_no_service <- 
  CTs %>%
  st_union() %>%
  st_erase(subway_service)

geom <- c(subway_service, subway_no_service)

subway_service_areas <-
  tibble(subway_service = c(TRUE, FALSE), 
         geometry = geom) %>% 
  st_as_sf()


## Clean up

rm(service_2013, service_2014, service_2015, service_2016, service_2017,
   service_2018, no_service_2013, no_service_2018, bike_expansion_2013to2018,
   growth_2014, growth_2015, growth_2016, growth_2017, growth_2018, bronx,
   subway_service, subway_no_service, geom)

