### MAP MAKING #################################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Initialize map list

figure <- list()

## Figure 1. Citi Bike service area expansion 2013-2018

figure[[1]] <- 
    tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                                xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                                relative = TRUE)) +
    tm_fill(col = "#f0f0f0") +
    tm_shape(nyc_city) +
    tm_fill(col = "grey80", title = "Base Map") +
    tm_shape(growth) +
    tm_polygons(col = "year", 
                palette = c("#ffd92f", "#a6d854", "#6bb2db", "#4fa35f",
                            "#db5727", "#ff9b30"),
                border.col = "#f0f0f0",
                title = "") +
    tm_shape(subway_lines) +
    tm_lines(col = "grey90", alpha = 0.75) +
    tm_layout(frame = TRUE,
              title = "Figure 1. Citi Bike service area expansion, 2013-2018",
              main.title.size = 1.5,
              legend.title.size = 1.2,
              legend.position = c("left", "top"),
              fontfamily = "Futura-Medium",
              title.fontfamily = "Futura-CondensedExtraBold") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[1]], "output/figure_1.png", width = 2400, height = 2400)


## Figure 2. Median household income

figure[[2]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(CTs) +
  tm_polygons(
    "med_income", 
    text = "No Data", 
    title = "Inside service area: $90,000\nOutside service area: $55,000", 
    border.alpha = 0,
    palette = "Greens",
    breaks = c(0, 20000, 40000, 60000, 80000, 100000, 150000, 200000, 260000)) +
  tm_shape(bike_service_areas[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 2. Median household income",
            legend.format = list(
              fun = function(x) paste0("$", formatC(x, digits = 0, format = "f", 
                                                    big.mark = ","))),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[2]], "output/figure_2.png", width = 2400, height = 2400)


## Figure 3. Poverty rate

figure[[3]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(CTs) +
  tm_polygons("poverty_pct",
              textNA = "No Data", 
              title = "Inside service area: 16.9%\nOutside service area: 20.3%", 
              border.alpha = 0,
              palette = c("#ef6548", "#fdbb84","#fdd49e","#fee8c8"),
              breaks = c(0,15,30,45,60)) +
  tm_shape(bike_service_areas[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 3. Poverty rate",
            legend.format = list(
              fun = function(x) paste0(formatC(x * 100, digits = 0, 
                                               format = "f"), "%")),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[3]], "output/figure_3.png", width = 2400, height = 2400)


## Figure 4. Non-hispanic white population

figure[[4]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(CTs) +
  tm_polygons("white_pct",
              textNA = "No Data", 
              title = "Inside service area: 52%\nOutside service area: 26%", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0,15,30,45,60)) +
  tm_shape(bike_service_areas[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 4. Non-hispanic white population",
            legend.format = list(
              fun = function(x) paste0(formatC(x * 100, digits = 0, 
                                               format = "f"), "%")),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[4]], "output/figure_4.png", width = 2400, height = 2400)


## Figure 5. Population with a bachelor's degree or more

figure[[5]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(CTs) +
  tm_polygons("education_pct",
              textNA = "No Data", 
              title = "Inside service area: 48%\nOutside service area: 19%", 
              border.alpha = 0,
              palette = "Blues") +
  tm_shape(bike_service_areas[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 5. Population with a bachelor's degree or more",
            legend.format = list(
              fun = function(x) paste0(formatC(x * 100, digits = 0, 
                                               format = "f"), "%")),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[5]], "output/figure_5.png", width = 2400, height = 2400)


## Figure 6. Bike sharing and subway access

fig_6_data <- st_intersection(subway_service_areas,bike_service_areas) %>% 
  filter(year == 2018) %>% 
  mutate(service = c("Both", "Access to bike sharing", "Access to subway",
                      "Neither"))

figure[[6]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(fig_6_data) +
  tm_polygons("service",
              palette = c("#b3cde3", "#decbe4", "#ccebc5", "#fbb4ae"),
              title = "",
              border.col = "#f0f0f0") +
  tm_layout(frame = TRUE,
            title = "Figure 6. Bike sharing and subway access",
            legend.format = list(
              fun = function(x) paste0(formatC(x, digits = 0, 
                                               format = "f"), "%")),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[6]], "output/figure_6.png", width = 2400, height = 2400)


## Figure 7. 

tm1 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("med_income",
              title = "",
              palette = "Greens",
              breaks = c(0,55000,91000,100000),
              labels = c("$54,688","$90,395","$99,792"),
              border.alpha = 0) +
  tm_legend(position = c("left","top"),
            text.size =2) +
  tm_layout(main.title.size = 1.5,
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold",
            main.title = "Median Household Income")

tm2 <-
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("poverty",
              title = "",
              palette = c("#fee8c8","#fdbb84","#ef6548"),
              border.alpha = 0,
              breaks = c(0,0.15,0.2,0.3),
              labels = c("14.9%","15.9%","20.3%"),
              legend.reverse = T) +
  tm_legend(position = c("left","top"),
            text.size =2) +
  tm_layout(main.title.size = 1.5,
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold",
            main.title = "Population in Poverty")

tm3 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("pop_white",
              title = "",
              palette = "Oranges",
              border.alpha = 0,
              breaks = c(0,0.3,0.52,0.6),
              labels = c("26.2%","51.8%","55.3%")) +
  tm_legend(position = c("left","top"),
            text.size =2) +
  tm_layout(main.title.size = 1.5,
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold",
            main.title = "White Population")

tm4 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("education",
              title = "",
              palette = "Blues",
              border.alpha = 0,
              breaks = c(0,0.2,0.48,0.55),
              labels = c("19.0%","47.5%","52.1%")) +
  tm_legend(position = c("left","top"),
            text.size =2) +
  tm_layout(main.title.size = 1.5,
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold",
            main.title = "Bachlor's Degree Attainment")

figure[[7]] <- tmap_arrange(tm1, tm2, tm3, tm4)
tmap_save(figure[[7]], "output/figure_7.png", width = 2400, height = 2400)


## Figure 8. Vulnerability index

figure[[8]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(CTs) +
  tm_fill("vulnerability_index",
          palette = "-RdYlGn",                 
          border.alpha = 0,
          title = "") +
  tm_shape(bike_service_areas[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 8. Vulnerability index",
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[8]], "output/figure_8.png", width = 2400, height = 2400)


# Figure 9. Expansion areas

figure[[9]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), 
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "#db5727", 
          title = "Bikeshare Expansion") +
  tm_shape(bike_service_filled) +
  tm_fill("#ffd92f") +
  tm_shape(subway_lines) +
  tm_lines(col = "grey90", alpha = 0.75) +
  tm_layout(frame = TRUE,
            title = "Figure 9. Proposed bike sharing expansion areas",
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "Existing Citi Bike service area",
                col = "#ffd92f") +
  tm_add_legend(type = "fill", labels = "Proposed expansion areas",
                col = "#db5727") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[9]], "output/figure_9.png", width = 2400, height = 2400)













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



  

