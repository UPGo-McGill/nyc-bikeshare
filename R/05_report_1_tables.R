### REPORT 1 TABLES AND FACTS ##################################################

## NYC summary demographics

nyc_demographics <- get_acs(
  geography = "county", 
  variables = c(pop_non_hisp_white = "B03002_003",
                pop_non_hisp_black = "B03002_004",
                pop_hisp = "B03002_012",
                education = "B16010_041",
                poverty = "B17001_002"),
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  summary_var = "B01003_001") %>% 
  set_names(c("GEOID", "NAME", "Variable", "Estimate", "MOE",
              "pop_total", "pop_total_MOE")) %>%
  select(-MOE, -pop_total_MOE, -GEOID, -NAME) %>% 
  spread(key = Variable, value = Estimate) %>%
  filter(pop_total > 100) %>%
  na.omit() %>% 
  summarize_all(sum)


### EXECUTIVE SUMMARY ####

## p. 2

# All facts taken from:

bike_service_comparison %>% st_drop_geometry()

## p. 3

# While 51.8% of people within Citi Bike’s service area are non-hispanic white,
# only 26.2% outside of the service area are white. Only 16.5% of people of
# color in New York City have access to bike sharing services, while 37.5% of
# white New Yorkers do.

# White bike share
(st_drop_geometry(bike_service_comparison)[4,3] *
    st_drop_geometry(bike_service_comparison)[4,4]) /
    nyc_demographics$pop_non_hisp_white

# Non white bike share
(st_drop_geometry(bike_service_comparison)[4,3] *
    (1 - st_drop_geometry(bike_service_comparison)[4,4])) / 
    (sum(nyc_demographics$pop_total) - nyc_demographics$pop_non_hisp_white)

# Only 94,000 New Yorkers who lack subway access live nearby Citi Bike stations;
# the vast majority (96.2%) do not have bike sharing access. But 95.3% of those
# with Citi Bike access also live close to the subway.  

subway_service_comparison %>% 
  st_drop_geometry() %>% 
  filter(subway_service == FALSE)

# 1.2 million New York residents have gained access to Citi Bike since 2013, but
# only 48,700 of them are underprivileged people lacking subway access. The
# remaining 1.1 million are only slightly more diverse and less affluent than 
# the original population with bike sharing, with a median household income of
# $84,900 (compared to $54,700 in the portion of the city without bike sharing
# access) and a poverty rate of 16.6% (compared with 20.2% in the area without 
# bike service), half are white (49.9% compared with 26.2% elsewhere), and 44.9%
# have at least a bachelor’s degree (compared to 19.0% elsewhere).

service_added <- 
  st_intersect_summarize(
  CTs,
  st_intersection(bike_service_growth_comparison["service"], 
                  subway_service_areas),
  group_vars = vars(service, subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income)) %>% st_drop_geometry()

service_added %>% 
  group_by(service) %>% 
  summarize(pop_total = sum(pop_total))


### 1. INTRODUCTION ####

## p. 7: Vulnerability index quantile

quantile(CTs$vulnerability_index, seq(0, 1, .333333))



### 2. EQUITY AND BIKE SHARING IN NEW YORK ####

## Table 1. Citi Bike service area expansion, 2013-2018

table_1 <- 
  st_intersect_summarize(
    CTs,
    tibble(
      year = 2013:2018,
      geometry = do.call(c,map(2013:2018, service_create, bike_distance))) %>% 
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




### 4. HOW HAS CITI BIKE ACCESS CHANGED SINCE 2013?

## Table 3. Demographic differences in bike sharing access, 2013-2018

table_3 <- 
  bike_service_growth_comparison %>% 
  st_drop_geometry() %>% 
  mutate_at(c("pop_white", "education", "poverty"), round, 3) %>% 
  mutate(pop_total = round(pop_total, -3),
         med_income = round(med_income, -2)) %>% 
  select(service, pop_total, med_income, poverty, pop_white, education)


### 5. WHICH NEIGHBORHOODS USE CITI BIKE, AND WHICH DON'T?

# p. 21: In the months of June and December of 2013, Citi Bike users took 1.02
# million rides—an average of 16,750 per day. Five years later in 2018, the
# equivalent numbers were 2.97 million rides in June and December, for an 
# average of 48,700 rides per day. The bike sharing network expanded
# substantially over these five years, but the average number of daily rides per
# station increased by 24.1%.

sum(stations_2018$rides) + sum(stations_2013$rides)
sum(stations_2018$rides) / 61
sum(stations_2013$rides) / 61

mean(stations_2018$rides) / mean(stations_2013$rides)

regression_2018 <-
  lm(ride_density ~ dist_to_broadway + pop_total + pop_white + education +
       poverty + med_income, data = voronoi_comparison_2018)

table_4 <- 
  summary(regression_2018)

# Table formatted using stargazer, with standard notation replacing absolutes
library(stargazer)
stargazer(regression_2018, type = "text")



### 6. HOW SHOULD NEW YORK'S BIKE SHARING EXPAND IN THE FUTURE?

# Table 5. Leading potential expansion areas based on vulnerability index

table_5 <- 
  target_neighbourhoods_demographics %>% 
  st_collection_extract("POLYGON") %>%
  group_by(nbhd) %>% 
  st_drop_geometry() %>%
  summarize(vulnerability_index = first(vulnerability_index),
            pop_total = first(pop_total),
            pop_no_subway = first(pop_no_subway) * first(pop_total))


# Table 6. Leading potential expansion areas based on subway access

table_6 <- 
  target_subway_access %>% 
  filter(subway_service == FALSE) %>%
  st_collection_extract("POLYGON") %>%
  group_by(nbhd) %>% 
  summarize(pop_total = first(pop_total),
            geometry = st_union(geometry)) %>%
  mutate(area = st_area(.) %>% set_units(mi^2),
         access_per_mi = pop_total / area) %>%
  st_drop_geometry()

