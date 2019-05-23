### MAP MAKING #################################################################

## Initialize map list and base map

figure <- list()
base_map <- tm_shape(nyc_msa, bbox = bb(st_bbox(nyc_city), xlim=c(-0.02, 1.02),
                                        ylim=c(0.01, 1.05), relative = TRUE),
                     unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_layout(frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")


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
    tm_layout(title = "Figure 1. Citi Bike service area expansion, 2013-2018")

tmap_save(figure[[1]], "output/figure_1.png", width = 2400, height = 2400)


## Figure 2. Median household income

figure[[2]] <- 
  base_map +
  tm_shape(CTs) +
  tm_polygons("med_income", border.alpha = 0, palette = "Greens",
    title = "Inside service area: $90,400\nOutside service area: $54,700", 
    breaks = c(0, 25000, 50000, 75000, 100000, 150000, 200000, 260000)) +
  tm_shape(bike_service_areas_no_holes[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(title = "Figure 2. Median household income",
            legend.format = list(fun = function(x) {
              paste0("$", formatC(x, digits = 0, format = "f", big.mark = ","))}
              )) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2)

tmap_save(figure[[2]], "output/figure_2.png", width = 2400, height = 2400)


## Figure 3. Poverty rate

figure[[3]] <- 
  base_map +
  tm_shape(CTs) +
  tm_polygons("poverty_pct", border.alpha = 0, 
              title = "Inside service area: 15.9%\nOutside service area: 20.3%", 
              palette = c("#ef6548", "#fdbb84","#fdd49e","#fee8c8"),
              breaks = c(0, .12, .24, .36, .48, .60)) +
  tm_shape(bike_service_areas_no_holes[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(title = "Figure 3. Poverty rate",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2)

tmap_save(figure[[3]], "output/figure_3.png", width = 2400, height = 2400)


## Figure 4. Non-hispanic white population

figure[[4]] <- 
  base_map +
  tm_shape(CTs) +
  tm_polygons("pop_white_pct",
              title = "Inside service area: 51.8%\nOutside service area: 26.2%", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, .12, .24, .36, .48, .60)) +
  tm_shape(bike_service_areas_no_holes[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(title = "Figure 4. Non-hispanic white population",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2)

tmap_save(figure[[4]], "output/figure_4.png", width = 2400, height = 2400)


## Figure 5. Population with a bachelor's degree or higher

figure[[5]] <- 
  base_map +
  tm_shape(CTs) +
  tm_polygons("education_pct",
              title = "Inside service area: 47.5%\nOutside service area: 19.0%", 
              border.alpha = 0,
              palette = "Blues") +
  tm_shape(bike_service_areas_no_holes[3,]) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(title = "Figure 5. Population with a bachelor's degree or higher",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "Citi Bike service area", col = "white",
                border.lwd = 2)

tmap_save(figure[[5]], "output/figure_5.png", width = 2400, height = 2400)


## Figure 6. Bike sharing and subway access

fig_6_data <-
  st_intersection(subway_service_areas, bike_service_areas_no_holes) %>% 
  filter(year == 2018) %>% 
  mutate(service = c("Both", "Access to bike sharing", "Access to subway",
                      "Neither"))

figure[[6]] <- 
  base_map +
  tm_shape(fig_6_data) +
  tm_polygons("service", title = "", border.col = "#f0f0f0", lwd = 2,
              palette = c("#b3cde3", "#decbe4", "#ccebc5", "#fbb4ae")) +
  tm_layout(title = "Figure 6. Bike sharing and subway access")

tmap_save(figure[[6]], "output/figure_6.png", width = 2400, height = 2400)


## Figure 7. Bike sharing service expansion demographics, 2013-2018

panel_base_map <- base_map +
  tm_layout(legend.text.size = 1.1, title.size = 1.1)
panel_base_map$tm_scale_bar <- NULL

tm1 <- panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("med_income", title = "", palette = "Greens",
              breaks = c(0, 55000, 90000, 100000),
              labels = c("$54,700","$83,700","$99,800"),
              border.alpha = 0) +
  tm_layout(title = "Median household income")

tm2 <-  panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("poverty", title = "", palette = c("#fee8c8","#fdbb84","#ef6548"),
              border.alpha = 0, breaks = c(0, 0.15, 0.2, 0.3),
              labels = c("14.9%", "16.9%", "20.3%"), legend.reverse = TRUE) +
  tm_layout(title = "Poverty rate")
 
tm3 <- 
  panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("pop_white", title = "", palette = "Oranges", border.alpha = 0,
              breaks = c(0, 0.3, 0.52, 0.6),
              labels = c("26.2%", "44.2%", "55.3%")) +
  tm_layout(title = "Non-hispanic white population")

tm4 <- panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("education", title = "", palette = "Blues", border.alpha = 0,
              breaks = c(0, 0.2, 0.48, 0.55),
              labels = c("19.0%", "44.5%", "52.1%")) +
  tm_layout(title = "Population with a bachelor's degree")

figure[[7]] <- tmap_arrange(tm1, tm2, tm3, tm4)
tmap_save(figure[[7]], "output/figure_7.png", width = 2400, height = 2400)


## Figure 8. Vulnerability index

figure[[8]] <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0, n = 8,
          title = "") +
  tm_shape(bike_service_filled) +
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
  tm_fill(col = "nbhd", title = "",
          palette = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                      "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
                      "#ffff99", "#b15928")) +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(bike_service_filled) +
  tm_fill(col = "grey40") +
  tm_shape(subway_lines) +
  tm_lines(col = "grey90", alpha = 0.75) +
  #tm_shape(target_neighbourhoods) +
  #tm_text("number", size = 0.7) +
  tm_layout(title = "Figure 9. Proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "Existing Citi Bike service area",
                col = "grey40", border.lwd = 0)

tmap_save(figure[[9]], "output/figure_9.png", width = 2400, height = 2400)


# Figure 10. Vulnerability of proposed bike sharing expansion areas

figure[[10]] <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0,
          title = "", n = 8) +
  tm_shape(st_erase(nyc_city, st_union(target_neighbourhoods))) +
  tm_fill(col = "grey80", alpha = 0.6) +
  tm_shape(target_neighbourhoods) +
  tm_borders(col = "white", lwd = 2) +
  tm_layout(title = "Figure 10. Vulnerability of proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0")

tmap_save(figure[[10]], "output/figure_10.png", width = 2400, height = 2400)


# Figure 11. Subway accessibility of proposed bike sharing expansion areas

figure[[11]] <- 
  base_map +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "pop_no_subway", palette = "viridis", alpha = 1,
          title = "Proportion of neighborhood population without subway access") +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(subway_service_areas[1,]) +
  tm_fill(col = "grey50", alpha = 0.3) +
  tm_layout(title = "Figure 11. Subway accessibility of proposed bike sharing expansion areas",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey50", 
                alpha = 0.3)

tmap_save(figure[[11]], "output/figure_11.png", width = 2400, height = 2400)


# Figure 12. Jackson Heights case study map

figure[[12]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(target_neighbourhoods[7,]), ext = 1.1)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(jhf_streets %>% filter(!is.na(name))) +
  tm_lines(col = "grey50") +
  tm_shape(st_erase(jhf_bike_catchment, jhf_subway_catchment)) +
  tm_fill(col = "red", alpha = 0.3) +
  tm_shape(jhf_subway_catchment) +
  tm_fill(col = "blue", alpha = 0.3) +
  tm_shape(subway_lines) +
  tm_lines(lwd = 1, alpha = 0.5) +
  tm_shape(jhf_stations) +
  tm_dots(size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "black") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "blue", 
                alpha = 0.3, border.alpha = 0.1, border.col = "blue") +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "red", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 12. Jackson Heights/Flushing case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure[[12]], "output/figure_12.png", width = 2400)


# Figure 13. South Bronx case study map

figure[[13]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(target_neighbourhoods[9,]), ext = 1.1)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(bronx_streets %>% filter(!is.na(name))) +
  tm_lines(col = "grey50") +
  tm_shape(st_erase(bronx_bike_catchment, bronx_subway_catchment)) +
  tm_fill(col = "red", alpha = 0.3) +
  tm_shape(bronx_subway_catchment) +
  tm_fill(col = "blue", alpha = 0.3) +
  tm_shape(subway_lines) +
  tm_lines(lwd = 1, alpha = 0.5) +
  tm_shape(bronx_stations) +
  tm_dots(size = 0.5) +
  #tm_text("stop_name", size = 0.7, 
  #        xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
  #        ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "black") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "blue", 
                alpha = 0.3, border.alpha = 0.1, border.col = "blue") +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "red", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 13. South Bronx case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure[[13]], "output/figure_13.png", height = 2400)


