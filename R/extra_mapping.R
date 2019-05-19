### Work-in-progress mapping









#rockaway
install.packages("osmdata")
library(osmdata)


plot(rockaway)

tm_shape(rockaway)+
  tm_fill(col ="#b2b2b2") + 
  
  ??osmdata


#demographic maps

{
  
  
  
  
  
  
  
  
  
  tm_shape(CTs) +
    tm_polygons ("immigrant_pct", 
                 textNA = "No Data", 
                 title = "Non-Citizen Population", 
                 border.alpha = 0,
                 palette = "-Oranges") +
    tm_shape(bike_service_filled) +
    tm_borders(col = "black", lwd = 2) +
    tm_layout(main.title = "Non-Citizen Population and Citi Bike Service Area", 
              inner.margins = .05, 
              frame = F,
              legend.outside = F,
              main.title.size = 2,
              legend.text.size = 1.1,
              legend.title.size = 1.5,
              legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
              legend.position = c(0.003, 0.65)) +
    tm_compass(position = c(.9, .05)) +
    tm_credits("Non-citizen population inside service area: 13%\nNon-citizen population outside service area: 18%",
               size = 1,
               position = c(0.003,.93)) +
    tm_add_legend(type = "fill", labels = "2018 Citi Bike Service Area", col = "white", border.lwd = 2) 
  
  
  
}


######carownership

{
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
  
  
  CTs_vehicles <- CTs_vehicles %>% filter(pop_total > 100) %>% na.omit()
  
  CTs_vehicles <- st_erase(CTs_vehicles, nyc_water)
  
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
                breaks = c(-60, -30, -15,  0, 0, 15, 30, 60),
                title = "Difference between Male and Female Car Ownership (%)", 
                palette = "RdYlBu") + 
    tm_shape(servicearea_2018) +
    tm_borders(col = "black", lwd = 2) + 
    tm_credits("Rates calculated by subtracting female ownership rates from male ownership rates", 
               position = c(0.007, 0.98)) +
    tm_layout(frame = F,
              main.title = "Vehicle Access Gender Gap",
              legend.position = c(0.007, 0.6))
  
  
}

# Expansion areas for bike service area

#med_income

tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Greens",
              breaks = c(0, 20000, 40000, 60000, 80000, 100000, 150000, 200000, 260000)) +
  tm_shape(expansion_bike_service_areas) +
  tm_fill(col = "black", alpha = 0.2)+ 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Income & Possible Expansion from Citi Bike Service Area", 
            main.title.size = 1.6,
            legend.title.size = 1.5,
            legend.text.size = 1.1,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))),
            legend.position = c(0.003, 0.6)) +
  tm_compass(position = c(.9, .05)) +
  tm_add_legend(type = "fill", labels = "2km buffer from Citi Bike stations", col = "#cccccc", border.col = "#cccccc") 


# race

tm_shape(CTs) +
  tm_polygons ("white_pct", 
               textNA = "No Data", 
               title = "White Population", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(expansion_bike_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(main.title = "Race & Possible Expansion from Citi Bike Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            main.title.size = 1.6,
            legend.title.size = 1.5,
            legend.text.size = 1.1,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.75)) +
  tm_compass(position = c(.9, .05))  +
  tm_add_legend(type = "fill", labels = "2km buffer from Citi Bike stations", col = "#cccccc", border.col = "#cccccc") 

# Expansion areas for subway service areas, med_income and race

# med_income

tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Greens",
              breaks = c(0, 20000, 40000, 60000, 80000, 100000, 150000, 200000, 260000)) +
  tm_shape(expansion_subway_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Income & Possible Expansion from Subway Service Area", 
            main.title.size = 1.6,
            legend.title.size = 1.5,
            legend.text.size = 1.1,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))),
            legend.position = c(0.007, 0.6)) +
  tm_compass(position = c(.9, .05)) +
  tm_add_legend(type = "fill", labels = "2km buffer from subway stations", col = "#cccccc", border.col = "#cccccc") 

# race

tm_shape(CTs) +
  tm_polygons ("white_pct", 
               textNA = "No Data", 
               title = "White Population", 
               alpha = 1,
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(expansion_subway_service_areas) +
  tm_fill(col = "black", alpha = 0.2) + 
  tm_layout(inner.margins = 0.05, 
            frame = F,
            main.title = "Race & Possible Expansion from Subway Service Area", 
            main.title.size = 1.6,
            legend.title.size = 1.5,
            legend.text.size = 1.1,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.75)) +
  tm_compass(position = c(.9, .05))  +
  tm_add_legend(type = "fill", labels = "2km buffer from subway stations", col = "#cccccc", border.col = "#cccccc") 



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
            main.title.size = 2,
            legend.title.size = 1.5,
            legend.text.size = 1.1,
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))





