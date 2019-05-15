### MAP MAKING #################################################################

#demographic maps

{
  
CTs$white_percent <- NA
CTs$white_percent <- (CTs$pop_white/ CTs$pop_total) * 100

CTs$education_percent <- NA
CTs$education_percent <- (CTs$education/ CTs$pop_total) * 100

CTs$immigrant_percent <- NA
CTs$immigrant_percent <- (CTs$immigrant/ CTs$pop_total) * 100



tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, 40000, 80000, 120000, 160000, 200000, 260000)) +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 3, alpha = 0.5) +
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Median Household Income and Citibike Service Area", 
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.text.size = .8,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f"))),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("Average inside service area: $90000\nAverage outside service area: $55000",
             size = 1,
             position = c(0.003,.93))
  

tm_shape(CTs) +
  tm_polygons ("white_percent", 
               textNA = "No Data", 
               title = "White Population", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 3, alpha = 0.5) +
  tm_layout(main.title = "Race and Citibike Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))+
  tm_credits("White population inside service area: 52%\nWhite population outside service area: 26%",
             size = 1,
             position = c(0.003,.93))

 
tm_shape(CTs) +
  tm_polygons ("education_percent", 
               textNA = "No Data", 
               title = "Population with Bachelor Degree or more", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 2) + 
  tm_layout(main.title = "Education and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05)) + 
  tm_credits("Average inside service area: 48%\nAverage outside service area: 19%",
             size = 1,
             position = c(0.003,.93))


tm_shape(CTs) +
  tm_polygons ("immigrant_percent", 
               textNA = "No Data", 
               title = "Non-Citizen Population", 
               border.alpha = 0,
               palette = "-Oranges") +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(main.title = "Non-Citizen Population and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.65)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("Non-citizen population inside service area: 13%\nNon-citizen population outside service area: 18%",
             size = 1,
             position = c(0.003,.93))

}



tm_shape(CTs) +
  tm_polygons ("poverty_percent", 
               textNA = "No Data", 
               title = "Poverty Level", 
               border.alpha = 0,
               palette = "-Oranges") +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(main.title = "Poverty and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.65)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("",
             size = 1,
             position = c(0.003,.93))


#bivariateservicemap

{
service_areas <- st_intersection(subway_service_areas,bike_service_areas)
bivariate_2018 <- service_areas %>% filter(year == 2018)
bivariate_2018$service <- c("Both", "Bike", "Subway", "Neither")


tm_shape(bivariate_2018) +
  tm_polygons("service",
              palette = c("#a6f72e", "#71aa3b", "#E6E6E6", "#fffaa3"),
              title = "",
              border.alpha = 0) +
  tm_layout(title = "Bike and Subway Service, 2018",
            frame = F,
            legend.position = c(0.2,.5),
            legend.text.size = 1) +
  tm_compass(position = c(.9, .05))

}

######carownership

#data import - TO BE MOVED


CTs_vehicles <- get_acs(
  geography = "tract", 
  variables = c(household_total = "B08201_001", 
                household_novehicle = "B08201_002",
                household_vehicle = c("B08201_003","B08201_004","B08201_005","B08201_006"),
                male_total = "B08014_008",
                male_novehicle = "B08014_009",
                male_vehicle = c("B08014_010","B08014_011","B08014_012","B08014_013", "B08014_014"),
                female_total = "B08014_015",
                female_novehicle = "B08014_016",
                female_vehicle = c("B08014_017","B08014_018","B08014_019","B08014_020", "B08014_021")),
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


CTs_vehicles <- st_erase(CTs_vehicles, ny_water)

names(CTs_vehicles) <- c("GEOID", "NAME", "Variable", "Estimate", "MOE", "pop_total",
                "pop_total_MOE", "geometry")


CTs_vehicles <-
  CTs_vehicles %>%
  select(-MOE, -pop_total_MOE) %>% 
  spread(key = Variable, value = Estimate)


CTs_vehicles$female_vehicle <- ((CTs_vehicles$female_vehicle1 + CTs_vehicles$female_vehicle2 + CTs_vehicles$female_vehicle3 + CTs_vehicles$female_vehicle4 + CTs_vehicles$female_vehicle5) / CTs_vehicles$female_total) * 100
CTs_vehicles$female_novehicle <- (CTs_vehicles$female_novehicle/CTs_vehicles$female_total)*100
CTs_vehicles$male_vehicle <- ((CTs_vehicles$male_vehicle1 + CTs_vehicles$male_vehicle2 + CTs_vehicles$male_vehicle3 + CTs_vehicles$male_vehicle4 + CTs_vehicles$male_vehicle5) / CTs_vehicles$male_total) * 100
CTs_vehicles$male_novehicle <- (CTs_vehicles$male_novehicle/CTs_vehicles$male_total)*100
CTs_vehicles$household_vehicle <- ((CTs_vehicles$household_vehicle1 + CTs_vehicles$household_vehicle2 + CTs_vehicles$household_vehicle3 + CTs_vehicles$household_vehicle4) / CTs_vehicles$household_total) * 100
CTs_vehicles$household_novehicle <- (CTs_vehicles$household_novehicle/CTs_vehicles$household_total)*100
CTs_vehicles$maleminusfemale <- CTs_vehicles$male_vehicle - CTs_vehicles$female_vehicle

CTs_vehicles <- select(CTs_vehicles, -female_vehicle1, -female_vehicle2, -female_vehicle3, -female_vehicle4, -female_vehicle5, 
                       -male_vehicle1, -male_vehicle2, -male_vehicle3, -male_vehicle4, -male_vehicle5,
                       -household_vehicle1, -household_vehicle2, -household_vehicle3, -household_vehicle4)


tm_shape(CTs_vehicles) +
  tm_polygons("household_vehicle",
              border.alpha = 0,
              title = "Households with Vehicle Access (%)") + 
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = F)

tm_shape(CTs_vehicles) +
  tm_polygons("maleminusfemale",
              border.alpha = 0,
              breaks = c(-60, -30, -25, -20, -15, -10, -5, 0, 0, 5, 10, 15, 20, 25, 30, 60),
              title = "Difference between Male and Female Car Ownership (%)", 
              palette = "RdYlBu") + 
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) + 
  tm_credits("Rates calculated by subtracting female ownership rates from male ownership rates", 
             position = c(0.007, 0.98)) +
  tm_layout(frame = F,
            main.title = "Vehicle Access Gender Gap",
            legend.position = c(0.007, 0.6))



tmaptools::palette_explorer()


# Expansion areas for bike service area, med_income

tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, 40000, 80000, 120000, 160000, 200000, 260000)) +
  tm_shape(expansion_bike_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Expansion Area from CitiBike Service Area", 
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.text.size = .8,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f"))),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))

# race

tm_shape(CTs) +
  tm_polygons ("white_percent", 
               textNA = "No Data", 
               title = "White Population", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(expansion_bike_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(main.title = "Expansion Area from Subway Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))

# Expansion areas for subway service areas, med_income and race

# med_income

tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, 40000, 80000, 120000, 160000, 200000, 260000)) +
  tm_shape(expansion_subway_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Expansion Area from Subway Service Area", 
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.text.size = .8,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f"))),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))

# race

tm_shape(CTs) +
  tm_polygons ("white_percent", 
               textNA = "No Data", 
               title = "White Population", 
               alpha = 1,
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(expansion_subway_service_areas) +
  tm_fill(col = "blue3", alpha = 0.07) + 
  tm_shape(expansion_subway_service_areas) +
  tm_borders(col = "blue3", lwd = 2, alpha = .3) +
  tm_layout(main.title = "Expansion Area from Subway Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))


#population density map


tm_shape(CTs) +
  tm_polygons ("pop_density",
               
               title = "Population Density", 
               border.alpha = 0,
  ) +
  tm_shape(subway_service_areas[1,]) +
  tm_fill(col = "black", alpha = 0.1) + 
  tm_layout(main.title = "Population Density",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))


#subway buffer demographics maps



tm_shape(city) + tm_fill(col = "grey") +
  tm_shape(subway_buffer_comparison) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "-Oranges",
              alpha = .5,
              breaks = c(20000, 40000, 60000, 80000, 100000, 120000, 140000)) +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 3, alpha = 0.5) + 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Vulnerability by Subway Stop", 
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.text.size = .8,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f"))),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))
  

tm_shape(city) + tm_fill(col = "grey") +
  tm_shape(subway_buffer_comparison) +
  tm_polygons ("pop_white", 
               textNA = "No Data", 
               title = "White Population", 
               border.alpha = 0,
               alpha = .5,
               palette = "-Oranges") +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 3, alpha = 0.5) +
  tm_layout(main.title = "Race and Citibike Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, format = "f"), "%")),
            legend.position = c(0.003, 0.7))+
  tm_compass(position = c(.9, .05))

