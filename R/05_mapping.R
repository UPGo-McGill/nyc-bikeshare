### MAP MAKING #################################################################

## Load libraries and helper functions

source("R/01_helper_functions.R")


## Initialize map list and base map

figure <- list()
base_map <-
  tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city),
                              xlim=c(-0.02, 1.02), ylim=c(0.01, 1.05), 
                              relative = TRUE)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map")


## Figure 1. Citi Bike service area expansion 2013-2018

figure[[1]] <- 
    base_map +
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
  base_map +
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
  base_map +
  tm_shape(CTs) +
  tm_polygons("poverty_pct",
              title = "Inside service area: 16.9%\nOutside service area: 20.3%", 
              border.alpha = 0,
              palette = c("#ef6548", "#fdbb84","#fdd49e","#fee8c8"),
              breaks = c(0, .15, .30, .45, .60)) +
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
  base_map +
  tm_shape(CTs) +
  tm_polygons("pop_white_pct",
              title = "Inside service area: 52%\nOutside service area: 26%", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, .15, .30, .45, .60)) +
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
  base_map +
  tm_shape(CTs) +
  tm_polygons("education_pct",
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
  base_map +
  tm_shape(fig_6_data) +
  tm_polygons("service", title = "", border.col = "#f0f0f0", lwd = 2,
              palette = c("#b3cde3", "#decbe4", "#ccebc5", "#fbb4ae")) +
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
  base_map +
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
  base_map +
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
  base_map +
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
  base_map +
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
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0, n = 8,
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
  base_map +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "nbhd", title = "", palette = "Set3", n = 12) +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(bike_service_filled) +
  tm_fill(col = "grey40") +
  tm_shape(subway_lines) +
  tm_lines(col = "grey90", alpha = 0.75) +
  #tm_shape(target_neighbourhoods) +
  #tm_text("number", size = 0.7) +
  tm_layout(frame = TRUE,
            title = "Figure 9. Proposed bike sharing expansion areas",
            legend.format = list(fun = function(x) paste0("1. ", x)),
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "Existing Citi Bike service area",
                col = "grey40", border.lwd = 0) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[9]], "output/figure_9.png", width = 2400, height = 2400)


# Figure 10. Vulnerability of proposed bike sharing expansion areas

figure[[10]] <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index",
          palette = "-RdYlGn",                 
          border.alpha = 0,
          title = "", n =  8) +
  tm_shape(st_erase(nyc_city, st_union(target_neighbourhoods))) +
  tm_fill(col = "grey80", alpha = 0.6) +
  tm_shape(target_neighbourhoods) +
  tm_borders(col = "white", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 10. Vulnerability of proposed bike sharing expansion areas",
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50")

tmap_save(figure[[10]], "output/figure_10.png", width = 2400, height = 2400)


