### TABLES AND CHARTS ##########################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


### 1. INTRODUCTION ####

# 12 priority areas table

target_subway_access <- st_intersect_summarize(
  CTs,
  target_subway_areas,
  group_vars = vars(nbhd, subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income))

## Vulnerability index quantile

quantile(CTs$vulnerability_index, seq(0, 1, .333333))



### 2. EQUITY AND BIKE SHARING IN NEW YORK ####

## Table 1. Citi Bike service area expansion, 2013-2018

table_1 <- 
  st_intersect_summarize(
    CTs,
    tibble(
      year = 2013:2018,
      geometry = do.call(c,map(2013:2018, service_create, 300))) %>% 
      st_as_sf(),
    group_vars = vars(year), population = pop_total, sum_vars = vars(pop_white),
    mean_vars = vars(med_income)) %>% 
  mutate(service_area_size = set_units(st_area(.), km^2),
         # Station numbers taken from December operating reports:
         # https://www.citibikenyc.com/system-data/operating-reports
         number_of_stations_at_end_of_year = c(338, 325, 456, 585, 740, 746)
         ) %>% 
  st_drop_geometry() %>% 
  select(year, population_in_service_area = pop_total, 
         number_of_stations_at_end_of_year, service_area_size) %>% 
  mutate(population_in_service_area = round(population_in_service_area, -2),
         service_area_size = round(service_area_size, 1))


### 3. WHO HAS ACCESS TO CITI BIKE, AND WHO DOESN'T?

## NYC racial demographics
 {nyc_demographics <- get_acs(
  geography = "county", 
  variables = c( 
                 pop_non_hisp_white = "B03002_003",
                pop_non_hisp_black = "B03002_004",
                pop_hisp = "B03002_012"),
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  summary_var = "B01003_001")

names(nyc_demographics) <- c("GEOID", "NAME", "Variable", "Estimate", "MOE", "pop_total",
                "pop_total_MOE")

nyc_demographics <-
  nyc_demographics %>%
  select(-MOE, -pop_total_MOE) %>% 
  spread(key = Variable, value = Estimate)

nyc_demographics <- nyc_demographics %>% filter(pop_total > 100) %>% na.omit() }

nyc_summary_demographics <- c(pop_total = sum(nyc_demographics$pop_total) ,
                              pop_white = round(sum(nyc_demographics$pop_non_hisp_white)/sum(nyc_demographics$pop_total), 3),
                              pop_black = round(sum(nyc_demographics$pop_non_hisp_black)/sum(nyc_demographics$pop_total), 3),
                              pop_hisp = round(sum(nyc_demographics$pop_hisp)/sum(nyc_demographics$pop_total), 3))

## Table 2. Demographic differences in bike sharing access

table_2 <-
  bike_service_comparison %>%
  st_drop_geometry() %>% 
  filter(year == 2018) %>% 
  mutate(subway_access = c(
    subway_service_comparison$pop_total[3] / pop_total[1], 
    subway_service_comparison$pop_total[4] / pop_total[2])) %>% 
    select(-immigrant, -vulnerability_index) %>% 
  mutate(pop_total = round(pop_total, -3),
         med_income = round(med_income, -2),
         pop_white = round(pop_white, 3),
         education = round(education, 3),
         poverty = round(poverty, 3),
         subway_access = round(subway_access, 3))



## Table X. Expansion neighborhood demographics

table_4 <-
  target_neighbourhoods_demographics %>% 
  st_drop_geometry() %>% 
    mutate_at(c("pop_white", "education", "poverty", "pop_no_subway"), round, 3) %>%
    mutate(pop_total = round(pop_total, -3),
           med_income = round(med_income, -2)) %>% 
    select(neighborhood = nbhd, population = pop_total, med_income,
           pct_in_poverty = poverty, pct_non_hispanic_white = pop_white,
           pct_with_bachelors_degree = education,
           pct_without_subway_access = pop_no_subway)
    