### DATA IMPORT ################################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Import and clean up station data

station_list <-
  st_read("data/station_list.csv", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  st_as_sf() %>% 
  select(-WKT) %>% 
  st_set_crs(26918) %>% 
  mutate(ID = as.numeric(ID), Year = as.numeric(Year))


## Import subway data and Public Use Microdata Areas

NY_pumas <- pumas(36) %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  mutate(PUMA_name = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10, -AWATER10, -INTPTLAT10, -INTPTLON10)

subway <-
  st_read("data", "stops_nyc_subway_nov2018") %>%  
  st_transform(26918) %>% 
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(borough = NAMELSAD) %>% 
  select(-NAMELSAD, -stop_id2, -GEOID, -stop_lat, -stop_lon)


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
  mutate(std_pov = scale(1 - (poverty/pop_total)),
         std_white = scale(pop_white/pop_total),
         std_ed = scale(education/pop_total),
         std_inc = scale(med_income),
         vulnerability_index = 4 - (index_create(std_pov) + 
                                      index_create(std_white) + index_create(std_ed) + 
                                      index_create(std_inc))) %>% 
  select(-std_pov, -std_white, -std_ed, -std_inc)



# Add additional variables

CTs$white_percent <- NA
CTs$white_percent <- (CTs$pop_white/ CTs$pop_total) * 100

CTs$education_percent <- NA
CTs$education_percent <- (CTs$education/ CTs$pop_total) * 100

CTs$immigrant_percent <- NA
CTs$immigrant_percent <- (CTs$immigrant/ CTs$pop_total) * 100

CTs$poverty_percent <- NA
CTs$poverty_percent <- (CTs$poverty/ CTs$pop_total) * 100


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
         geometry = 
           c(service_2013, no_service_2013, service_2018, no_service_2018)) %>%
  st_as_sf()

bike_expansion_2013to2018 <- service_2018 %>% 
  st_erase(service_2013)


## Create service areas for 2014-2016 for service area growth

service_2014 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2014) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

service_2015 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2015) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

service_2016 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2016) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

service_2017 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2017) %>%
                     st_buffer(300) %>%
                     st_union() %>%
                     st_erase(ny_water) )

growth_2018 <- st_difference(service_2018, service_2017)
growth_2017 <- st_difference(service_2017, service_2016)
growth_2016 <- st_difference(service_2016, service_2015)
growth_2015 <- st_difference(service_2015, service_2014)
growth_2014 <- st_difference(service_2014, service_2013)

growth_2017 <- st_erase(growth_2017, (filter(counties, NAME == "Bronx County, New York")))


service_2013 <- st_intersect_summarize(
  CTs,
  service_2013,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


service_2014 <- st_intersect_summarize(
  CTs,
  service_2014,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


service_2015 <- st_intersect_summarize(
  CTs,
  service_2015,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


service_2016 <- st_intersect_summarize(
  CTs,
  service_2016,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


service_2017 <- st_intersect_summarize(
  CTs,
  service_2017,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


service_2018 <- st_intersect_summarize(
  CTs,
  service_2018,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))

summary_serviceareas <- rbind(service_2013,service_2014,service_2015,service_2016,service_2017,service_2018)
summary_serviceareas$year <- c("2013", "2014", "2015", "2016", "2017", "2018")


no_service_2018 <- st_intersect_summarize(
  CTs,
  no_service_2018,
  group_vars = NA,
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income))


summary_serviceareas_no_service <-rbind(service_2013, service_2018, no_service_2018)
summary_serviceareas_no_service <- mutate(summary_serviceareas_no_service, "NA" = c("service_2013", "service_2018", "no_service") )   

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

rm(subway_service, subway_no_service, geom, ny_water)

