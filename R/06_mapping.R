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
              palette = "-Purples",
              breaks = c(0, .10, .20, .30, .40, .70)) +
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
  tm_layout(title = "Figure 4. Non-Hispanic white population",
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
  tm_polygons("poverty", title = "", palette = "Purples",
              border.alpha = 0, breaks = c(0, 0.15, 0.2, 0.3),
              labels = c("14.9%", "16.9%", "20.3%"), legend.reverse = TRUE) +
  tm_layout(title = "Poverty rate")
 
tm3 <- 
  panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("pop_white", title = "", palette = "Oranges", border.alpha = 0,
              breaks = c(0, 0.3, 0.52, 0.6),
              labels = c("26.2%", "44.2%", "55.3%")) +
  tm_layout(title = "Non-Hispanic white population")

tm4 <- panel_base_map +
  tm_shape(bike_service_growth_comparison) +  
  tm_polygons("education", title = "", palette = "Blues", border.alpha = 0,
              breaks = c(0, 0.2, 0.48, 0.55),
              labels = c("19.0%", "44.5%", "52.1%")) +
  tm_layout(title = "Population with a bachelor's degree")

figure[[7]] <- tmap_arrange(tm1, tm2, tm3, tm4)
tmap_save(figure[[7]], "output/figure_7.png", width = 2400, height = 2400)


## Figure 8. Sample ride density

ride_base_map <-
  tm_shape(nyc_msa,
           bbox = bb(st_bbox(voronoi_2018), ext = 1, relative = TRUE),
           unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(subway_lines) +
  tm_lines(col = "grey90", alpha = 0.75) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_layout(frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold",
            legend.format = list(fun = function(x) {
              paste0(formatC(x, digits = 0, format = "f", big.mark = ","))
            }))

ride_map_2013 <-
  ride_base_map +
  tm_shape(voronoi_2013) +
  tm_polygons("rides", convert2density = TRUE, style = "fixed", n = 7,
              breaks = c(0, 600, 1200, 1800, 3000, 4500, 6000,
                         Inf),
              palette = "viridis", border.col = "white", border.alpha = 0.2,
              title = "") +
  tm_shape(stations_2013) +
  tm_dots(col = "white", alpha = 0.2) +
  tm_layout(title = "2013")

ride_map_2018 <- 
  ride_base_map +
  tm_shape(voronoi_2018) +
  tm_polygons("rides", convert2density = TRUE, style = "fixed", n = 7,
              breaks = c(0, 600, 1200, 1800, 3000, 4500, 6000,
                         Inf),
              palette = "viridis", border.col = "white", border.alpha = 0.2,
              title = "") +
  tm_shape(stations_2018) +
  tm_dots(col = "white", alpha = 0.2) +
  tm_layout(title = "2018")

figure[[8]] <- tmap_arrange(ride_map_2013, ride_map_2018)
tmap_save(figure[[8]], "output/figure_8.png", width = 2400)


## Figure 9. Ride density correlations

scatter_base <- 
  voronoi_comparison_2018 %>% 
  st_drop_geometry() %>% 
  ggplot(aes(y = ride_density)) +
  scale_y_log10() +
  labs(y = "Ride density (log)") +
  theme_minimal() +
  theme(text=element_text(family = "Futura-Medium"), legend.position = "none")

sp0 <- 
  scatter_base +
  geom_point(aes(pop_total, size = rides), colour = "black", alpha = 0.3) +
  geom_smooth(aes(pop_total), colour = "black", method = "lm", se = FALSE) +
  scale_x_continuous(name = "Total population", labels = comma) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = -0.24", size = 3,
           colour = "black", family = "Futura-Medium", hjust = 0)

sp1 <- 
  scatter_base +
  geom_point(aes(med_income, size = rides), colour = "#2E974E", alpha = 0.3) +
  geom_smooth(aes(med_income), colour = "#2E974E", method = "lm", se = FALSE) +
  scale_x_continuous(name = "Median household income", labels = dollar) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = 0.21", size = 3,
           colour = "#2E974E", family = "Futura-Medium", hjust = 0)
  
sp2 <- 
  scatter_base +
  geom_point(aes(poverty, size = rides), colour = "#7262AC", alpha = 0.3) +
  geom_smooth(aes(poverty), colour = "#7262AC", method = "lm", se = FALSE) +
  scale_x_continuous(name = "Poverty rate", labels = percent) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = -0.11",
           size = 3, colour = "#7262AC", family = "Futura-Medium", hjust = 0)

sp3 <- 
  scatter_base +
  geom_point(aes(pop_white, size = rides), colour = "#E25508", alpha = 0.3) +
  geom_smooth(aes(pop_white), colour = "#E25508", method = "lm", se = FALSE) +
  scale_x_continuous(name = "Non-Hispanic white population", labels = percent) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = 0.15", size = 3,
           colour = "#E25508", family = "Futura-Medium", hjust = 0)

sp4 <- 
  scatter_base +
  geom_point(aes(education, size = rides), colour = "#2E7EBB", alpha = 0.3) +
  geom_smooth(aes(education), colour = "#2E7EBB", method = "lm", se = FALSE) +
  scale_x_continuous(name = "Population with a bachelor's degree",
                     labels = percent) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = 0.23", size = 3,
           colour = "#2E7EBB", family = "Futura-Medium", hjust = 0)

sp5 <- 
  scatter_base +
  geom_point(aes(dist_to_broadway *.00062, size = rides), colour = "black",
             alpha = 0.3) +
  geom_smooth(aes(dist_to_broadway *.00062), colour = "black", method = "lm",
              se = FALSE) +
  scale_x_continuous(name = "Distance to Broadway",
                     labels = unit_format(unit = "mi", big.mark = ",")) +
  annotate("text", x = 0, y = 1e-05, label = "Correlation = -0.33", size = 3,
           colour = "black", family = "Futura-Medium", hjust = 0)


legend <- get_legend(scatter_base + 
                       geom_point(aes(pop_density, size = rides), alpha = 0.3) +
                       scale_size(labels = comma) +
                       labs(size = "Ride count") +
                       theme(legend.position = "bottom"))

figure[[9]] <-
  grid.arrange(sp0, sp1, sp2, sp3, sp4, sp5, legend, heights = c(1, 1, 0.2), 
               layout_matrix = rbind(c(1, 2, 3), c(4, 5, 6), c(7, 7, 7)))

ggsave("output/figure_9.png", plot = figure[[9]], width = 12.8, height = 8,
       units = "in", dpi = 300)


## Figure 10. Vulnerability index

figure[[10]] <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0, n = 8,
          title = "") +
  tm_shape(bike_service_filled) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = TRUE,
            title = "Figure 10. Vulnerability index",
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

tmap_save(figure[[10]], "output/figure_10.png", width = 2400, height = 2400)


# Figure 11. Expansion areas

figure[[11]] <- 
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
  tm_layout(title = "Figure 11. Proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "Existing Citi Bike service area",
                col = "grey40", border.lwd = 0)

tmap_save(figure[[11]], "output/figure_11.png", width = 2400, height = 2400)


# Figure 12. Vulnerability of proposed bike sharing expansion areas

figure[[12]] <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0,
          title = "", n = 8) +
  tm_shape(st_erase(nyc_city, st_union(target_neighbourhoods))) +
  tm_fill(col = "grey80", alpha = 0.6) +
  tm_shape(target_neighbourhoods) +
  tm_borders(col = "white", lwd = 2) +
  tm_layout(title = "Figure 12. Vulnerability of proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0")

tmap_save(figure[[12]], "output/figure_12.png", width = 2400, height = 2400)


# Figure 13. Subway accessibility of proposed bike sharing expansion areas

figure[[13]] <- 
  base_map +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "pop_no_subway", palette = "viridis", alpha = 1,
          title = "Proportion of neighborhood population without subway access") +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(subway_service_areas[1,]) +
  tm_fill(col = "grey50", alpha = 0.3) +
  tm_layout(title = "Figure 13. Subway accessibility of proposed bike sharing expansion areas",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey50", 
                alpha = 0.3)

tmap_save(figure[[13]], "output/figure_13.png", width = 2400, height = 2400)

