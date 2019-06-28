### REPORT 2 MAPS (IN GGPLOT2) #################################################

## NYC base map

base_map_gg <- 
  ggplot() +
  geom_sf(data = nyc_msa, fill = "#F0F0F0", lwd = 0) +
  geom_sf(data = nyc_city, fill = "grey80", lwd = 0) +
  annotation_scale(location = "br", width_hint = 0.4, line_col = "grey50",
                   bar_cols = c("grey50", "white"), unit_category = "imperial",
                   style = "ticks") +
  theme(legend.position = c(0.01, .99),
        legend.justification = c(0, 1),
        legend.spacing = unit(0, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        text = element_text(family = "Futura-Medium"),
        title = element_text(family = "Futura-CondensedExtraBold", size = 15),
        strip.text = element_text(family = "Futura-Bold", size = 12),
        legend.title = element_text(family = "Futura-CondensedExtraBold", 
                                    size = 15),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) 


## Figure 1. Expansion areas

figure_2_01 <- 
  base_map_gg +
  geom_sf(data = target_neighbourhoods, mapping = aes(fill = nbhd), 
          colour = "white") +
  geom_sf(data = st_sf(bike = "Existing Citi Bike service area", 
                       geometry = bike_service_areas_no_holes$geometry[3]),
          mapping = aes(colour = bike), fill = "grey40") +
  geom_sf(data = subway_lines, colour = alpha("grey90", 0.75)) +
  scale_fill_manual(name = "Figure 1. Proposed bike sharing expansion areas",
                    values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                               "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
                               "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")) +
  scale_colour_manual(name = NULL, values = "white") +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +
  gg_bbox(nyc_city, 0.01, .99, 0.04, 1.02)
  
ggsave("output/report-2/figure_2_01.png", plot = figure_2_01,
       height = 8, units = "in", dpi = 300)


## Figure 2. Vulnerability of proposed bike sharing expansion areas

figure_2_02 <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0,
          title = "", n = 8) +
  tm_shape(st_erase(nyc_city, st_union(target_neighbourhoods))) +
  tm_fill(col = "grey80", alpha = 0.6) +
  tm_shape(target_neighbourhoods) +
  tm_borders(col = "white", lwd = 2) +
  tm_layout(
    title = "Figure 2. Vulnerability of proposed bike sharing expansion areas"
  ) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0")

tmap_save(figure_2_02, "output/report-2/figure_2_02.png", width = 2400, 
          height = 2400)


## Figure 3. Subway accessibility of proposed bike sharing expansion areas

figure_2_03 <- 
  base_map +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "pop_no_subway", palette = "viridis", alpha = 1,
          title = "Proportion of neighborhood population without subway access") +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(subway_service_areas[1,]) +
  tm_fill(col = "grey50", alpha = 0.3) +
  tm_layout(title = "Figure 3. Subway accessibility of proposed bike sharing expansion areas",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"), "%")})) +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey50", 
                alpha = 0.3)

tmap_save(figure_2_03, "output/report-2/figure_2_03.png", width = 2400,
          height = 2400)


































rbind(mutate(voronoi_2013, year = 2013), mutate(voronoi_2018, year = 2018)) %>% 
  mutate(area = set_units(st_area(geometry), mi^2),
         rides_per_sq_mi = rides / area) %>% 
  ggplot() +
  geom_sf(data = nyc_msa, fill = "#F0F0F0", lwd = 0) +
  geom_sf(data = nyc_city, fill = "grey80", lwd = 0) +
  geom_sf(data = subway_lines, colour = "white", alpha = 0.75) +
  geom_sf(aes(fill = drop_units(rides_per_sq_mi)),
          colour = alpha("white", 0.2)) +
  geom_sf(data = rbind(mutate(stations_2013, year = 2013), 
                       mutate(stations_2018, year = 2018)), 
          colour = "white", size = 0.5, alpha = 0.2) +
  annotation_scale(location = "br", width_hint = 0.4, line_col = "grey50",
                   bar_cols = c("grey50", "white"), unit_category = "imperial",
                   style = "ticks") +
  scale_fill_viridis(name = "Daily rides per square mile",
                     limits = c(0, 6000), oob = scales::squish) +
  coord_sf(xlim = st_bbox(voronoi_2018)[c(1,3)], 
           ylim = st_bbox(voronoi_2018)[c(2,4)]) +
  ggtitle("Figure 8. Citi Bike ride density") +
  facet_wrap(vars(year)) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        text = element_text(family = "Futura-Medium"),
        title = element_text(family = "Futura-CondensedExtraBold", size = 15),
        strip.text = element_text(family = "Futura-Bold", size = 12),
        legend.title = element_text(family = "Futura-Bold", size = 10),
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) 



plot_gg(ggmap, multicore = TRUE, width = 5, height = 5, scale = 250)


st_bbox(voronoi_2018)[c(1,3)]
