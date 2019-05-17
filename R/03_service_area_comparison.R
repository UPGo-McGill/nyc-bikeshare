### SERVICE AREA COMPARISON ####################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Intersect CTs with service areas

bike_service_comparison <- st_intersect_summarize(
  CTs,
  bike_service_areas,
  group_vars = vars(year, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education, poverty),
  mean_vars = vars(med_income)
)


subway_service_comparison <- st_intersect_summarize(
  CTs,
  subway_service_areas,
  group_vars = vars(subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)


## Compare areas with/without transit which got bike sharing

bike_service_added <-
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas) %>% 
  filter(bike_service == TRUE)

bike_comparison2018 <- st_intersect_summarize(
  CTs,
  bike_service_added,
  group_vars = vars(subway_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)

# 2. Compare bikeshare access and subway access together vs people with no access to either

transit_access2018 <- 
  bike_service_added <-
  st_intersection(
    filter(bike_service_areas, year == 2018),
    subway_service_areas)

transit_access2018 <- st_intersect_summarize(
  CTs,
  transit_access2018,
  group_vars = vars(subway_service, bike_service),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income)
)


# Identify possible locations for future Citibike expansion

# take subway service area, add buffers for potential expansion, and subtract 800m buffers


expansion_subway_service_areas <- 
  suppressWarnings(subway %>%
                     st_buffer(2000) %>%
                     st_union() %>% 
                     st_erase(subway_service_areas[1,]) )

expansion_bike_service_areas <- 
  suppressWarnings(bike_service_areas[3,] %>% 
                     st_buffer(2000) %>% 
                     st_union() %>% 
                     st_erase(bike_service_areas[3,])) 

expansion_subway_service_areas <- expansion_subway_service_areas %>% st_intersection(city)%>%  st_collection_extract("POLYGON")


#remove parks from within bike service area          

bike_service_filled<- fill_holes(bike_service_areas$geometry[3], 10000000)

# Take citibike service area, add buffers for potential expansion, and subtract 300m buffers

expansion_bike_service_areas <- station_list %>%
  filter(Year == 2018) %>%
  st_buffer(2000)%>%
  st_union() %>%
  st_erase(bike_service_filled) 

expansion_bike_service_areas <- expansion_bike_service_areas %>% st_intersection(city)%>%  st_collection_extract("POLYGON")

# create 2000m subway buffers with demographic information for each subway stop
subway_buffers <- subway %>%
  st_buffer(2000)  %>%
  mutate(bike_service_proximity = st_distance(subway, bike_service_filled)) # adds column of distance between metro station and the 2018 citibike service area

subway_buffer_comparison <- st_intersect_summarize(
  CTs,
  subway_buffers,
  group_vars = vars(stop_name, stop_id, borough),
  population = pop_total,
  sum_vars = vars(pop_white, immigrant, education),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_comparison <- subway_buffer_comparison %>% 
  mutate (vulnerability_index = as.double(vulnerability_index))

subway_buffer_vulnerability2.75 <- subway_buffer_comparison %>% filter(vulnerability_index > 2.75)

subway_buffer_vulnerability2.75 <- subway %>% 
  filter(stop_id %in% subway_buffer_vulnerability2.75$stop_id) %>%
  st_buffer(2000) %>% 
  st_intersection(city) %>% 
  st_erase(bike_service_filled)

subway_buffer_vulnerability2.75 <- st_intersection(NY_pumas, subway_buffer_vulnerability2.75) 
plot(subway_buffer_vulnerability2.75[1])

subway_service <- subway_service_areas[1]

target_neighbourhoods <- subway_buffer_vulnerability2.75 %>% st_union() %>% st_collection_extract("POLYGON")

plot(target_neighbourhoods)

target_neighbourhoods_demographics <- st_intersect_summarize(
  CTs,
  target_neighbourhoods,
  group_vars = vars(NA),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))
st_area(target_neighbourhoods_demographics)
plot(target_neighbourhoods_demographics)

#create target neighbourhood geography

rockaway <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04114") %>% st_union()

jamaica <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04112" | PUMACE10 == "04111" |PUMACE10 == "04113"| PUMACE10 == "04106")

flushing <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04103")

sunset_park_bay_bridge <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04012" | PUMACE10 == "04013" |PUMACE10 == "04014")

jackson_heights_corona <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04112" | PUMACE10 == "04111" |PUMACE10 == "04113"| PUMACE10 == "04106")

jackson_heights_corona <- subway_buffer_vulnerability2.75 %>% filter(PUMACE10 == "04112" | PUMACE10 == "04111" |PUMACE10 == "04113"| PUMACE10 == "04106")



#get demographics by target neighbourhoods
subway_buffer_vulnerability2.75 <- st_intersect_summarize(
  CTs,
  subway_buffer_vulnerability2.75,
  group_vars = vars(PUMACE10, PUMA_name),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))

subway_buffer_vulnerability2.75 <- subway_buffer_vulnerability2.75 %>% mutate (vulnerability_index = as.double(vulnerability_index), pop_total = as.double(pop_total))  %>% filter(pop_total>1000) 


