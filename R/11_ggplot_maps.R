### REPORT 2 MAPS (IN GGPLOT2) #################################################

## Neighbourhood colours

nbhd_colours <- 
  c("#6caed1", "#1f78b4", "#8acf4e", "#33a02c", "#fb9a99", "#e31a1c",
    "#fdbf6f", "#ff7f00", "#a880bb", "#6a3d9a", "#ffff99", "#b15928")

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


## Network map template

network_template <-
  function(nbhd_value, 
           x_values = 0, 
           y_values = 0, 
           col_scale = 1, 
           bbox = c(0, 1, 0, 1), 
           title) {
  
  middle_colour_1 <- nbhd_colours[nbhd_value] %>% 
    str_sub(2, -5) %>% 
    strtoi(16) %>% 
    `*`(col_scale) %>% 
    `+`(strtoi("BF", 16)) %>% 
    `/`(col_scale + 1) %>% 
    round() %>% 
    as.hexmode()
  
  middle_colour_2 <- nbhd_colours[nbhd_value] %>% 
    str_sub(4, -3) %>% 
    strtoi(16) %>% 
    `*`(col_scale) %>% 
    `+`(strtoi("BF", 16)) %>% 
    `/`(col_scale + 1) %>% 
    round() %>% 
    as.hexmode()
  
  middle_colour_3 <- nbhd_colours[nbhd_value] %>% 
    str_sub(6, -1) %>% 
    strtoi(16) %>% 
    `*`(col_scale) %>% 
    `+`(strtoi("BF", 16)) %>% 
    `/`(col_scale + 1) %>% 
    round() %>% 
    as.hexmode()
  
  middle_colour <- 
    paste0("#", middle_colour_1, middle_colour_2, middle_colour_3)
  
  network_geom <- 
    st_sf(service = factor(c("Access to subway", "Access to bike sharing", 
                             "Access to both"),
                           levels = c("Access to subway",
                                      "Access to bike sharing",
                                      "Access to both")), 
          geometry = c(networks[[nbhd_value]][[2]], networks[[nbhd_value]][[1]],
                       networks[[nbhd_value]][[3]]))
  
  ggplot() +
    # NYC MSA
    geom_sf(data = nyc_msa, fill = "#F0F0F0", lwd = 0) +
    # NYC background
    geom_sf(data = nyc_city, colour = "grey90") +
    # Parks
    geom_sf(data = osm_networks[[nbhd_value]][[1]], fill = "#BDD490", 
            lwd = 0) +
    # Roads
    geom_sf(data = osm_networks[[nbhd_value]][[2]],
            colour = alpha("grey80", 0.7), lwd = 0.25) +
    # Service areas
    geom_sf(data = network_geom, mapping = aes(fill = service), 
            colour = alpha("white", 0.5), lwd = 0.2) +
    # Water
    geom_sf(data = nyc_water, fill = "#E5F7FF",
            colour = alpha("#326CAB", 0.15)) +
    # Subway background
    geom_sf(data = subway_lines, colour = "white", lwd = 1.3) +
    geom_sf(data = subway_stations, colour = "white",
            size = 2.7) +
    # Subway lines
    geom_sf(data = subway_lines, colour = "grey60", lwd = 1.2) +
    # Subway stations
    geom_sf(data = subway_stations, colour = "grey60", size = 2.5) +
    geom_sf(data = osm_networks[[nbhd_value]][[3]],
            colour = "white", size = 3.3) +
    geom_sf(data = mutate(osm_networks[[nbhd_value]][[3]], 
                          label = "Target subway station"),
            mapping = aes(colour = label), size = 3, show.legend = "point") +
    # Subway station labels
    geom_text_repel(data = osm_networks[[nbhd_value]][[3]],
                    mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                  y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                  label = stop_name),
                    nudge_x = x_values,
                    nudge_y = y_values,
                    family = "Futura-Medium",
                    segment.size = .4,
                    min.segment.length = 0,
                    point.padding = 1) +
    # Map scale
    annotation_scale(location = "br", width_hint = 0.4, line_col = "grey50",
                     bar_cols = c("grey50", "white"), 
                     unit_category = "imperial", style = "ticks") +
    # Map title
    ggtitle(title) +
    # Bounding box
    gg_bbox(networks[[nbhd_value]][[1]], bbox[1], bbox[2], bbox[3], bbox[4]) +
    # Scales, guides and themes
    scale_fill_manual(name = NULL, values = c(alpha("grey75", 0.7),  
                                              nbhd_colours[nbhd_value],
                                              middle_colour)) +
    scale_colour_manual(name = NULL, values = nbhd_colours[nbhd_value]) +
    guides(fill = guide_legend(order = 1,
                               override.aes = list(colour = "black", 
                                                   lwd = 0.5,
                                                   shape = 32)),
           colour = guide_legend(order = 2,
                                 override.aes = list(
                                   shape = 21, size = 5, colour = "white",
                                   fill = nbhd_colours[nbhd_value]))) +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.spacing = unit(0, "pt"),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          rect = element_blank(),
          text = element_text(family = "Futura-Medium"),
          title = element_text(family = "Futura-CondensedExtraBold", size = 15),
          strip.text = element_text(family = "Futura-Bold", size = 12),
          legend.title = element_text(family = "Futura-CondensedExtraBold", 
                                      size = 15),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) 
  
}


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
                    values = nbhd_colours) +
  scale_colour_manual(name = NULL, values = "white") +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +
  gg_bbox(nyc_city, 0.01, .99, 0.04, 1.02)
  
ggsave("output/report-2/figure_2_01.png", plot = figure_2_01, height = 8,
       width = 8, units = "in", dpi = 300)


## Figure 2. Vulnerability of bike sharing expansion areas

figure_2_02 <- 
  base_map_gg +
  geom_sf(data = CTs, mapping = aes(fill = vulnerability_index), lwd = 0) +
  geom_sf(data = st_erase(nyc_city, st_union(target_neighbourhoods)),
          fill = alpha("grey80", 0.6), lwd = 0) +
  geom_sf(data = target_neighbourhoods, fill = NA, colour = "white") +
  scale_fill_gradientn(
    name = "Figure 2. Vulnerability of bike sharing expansion areas",
    colours = get_brewer_pal(palette = "-RdYlGn", n = 8), limits = c(0, 4),
    labels = c("0", "< 1.0", "< 2.0", "< 3.0", "< 4.0")) +
  guides(fill = guide_legend()) +
  gg_bbox(nyc_city, 0.01, .99, 0.04, 1.02)

ggsave("output/report-2/figure_2_02.png", plot = figure_2_02, height = 8,
       width = 8, units = "in", dpi = 300)


## Figure 3. Subway accessibility of bike sharing expansion areas

figure_2_03 <- 
  base_map_gg +
  geom_sf(data = target_neighbourhoods, mapping = aes(fill = pop_no_subway),
          colour = "white") +
  geom_sf(data = mutate(subway_service_areas[1,], label = "Access to subway"), 
          mapping = aes(colour = label),
          fill = alpha("grey50", 0.3), lwd = 0) +
  scale_fill_viridis(
    limits = c(0, .4), labels = c("0%", "< 10%", "< 20%", "< 30%", "< 40%"),
    name = "Figure 3. Subway accessibility of bike sharing expansion areas") +
  scale_colour_manual(name = NULL, values = "grey50") +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2, 
                               override.aes = list(fill = "grey50"))) +
  gg_bbox(nyc_city, 0.01, .99, 0.04, 1.02)

ggsave("output/report-2/figure_2_03.png", plot = figure_2_03, height = 8,
       width = 8, units = "in", dpi = 300)

  
## Figure 4. Jackson Heights/Flushing

figure_2_04 <- 
  network_template(nbhd_value = 7,
                   x_values = 0,
                   y_values = c(350, -300, 0, 0, 0, 0, 200),
                   col_scale = 1,
                   bbox = c(0, 1, -0.05, 1.05),
                   title = "Figure 4. Jackson Heights/Flushing bike sharing")

ggsave("output/report-2/figure_2_04.png", plot = figure_2_04, width = 8,
       height = 8, units = "in", dpi = 300)


##  Figure 5. South Bronx

figure_2_05 <- 
  network_template(nbhd_value = 9, 
                   x_values = 0, 
                   y_values = 0, 
                   col_scale = 1.3,
                   bbox = c(-0.13, 1.13, 0, 1),
                   title = "Figure 5. South Bronx bike sharing")

figure_2_05$layers[[13]] <-
  geom_text_repel(data = filter(osm_networks[[9]][[3]], 
                                !(stop_id %in% c("414", "415"))), 
                  mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                label = stop_name),
                  nudge_x = c(0, -500, -450, 0, -250, 0, -350, -350, 0, 700,
                              650, 450, 950, 0, 0, -350, 350, 350, 0),
                  nudge_y = c(200, 200, 200, -200, -200, 250, 0, 0, 100, 0, 0,
                              0, 0, -150, 0, -200, 0, 0, -150),
                  family = "Futura-Medium",
                  segment.size = .4,
                  min.segment.length = 0,
                  point.padding = 1)

ggsave("output/report-2/figure_2_05.png", plot = figure_2_05, width = 8, 
       height = 8, units = "in", dpi = 300)


## Figure 6. Bushwick/Ridgewood

figure_2_06 <- 
  network_template(nbhd_value = 1, 
                   x_values = 0, 
                   y_values = c(0, 0, 0, 0, 300, 0, 0, 0),
                   col_scale = 1.2,
                   bbox = c(0, 1, -0.05, 1.05),
                   title ="Figure 6. Bushwick/Ridgewood bike sharing")

ggsave("output/report-2/figure_2_06.png", plot = figure_2_06, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 7. Central Bronx

figure_2_07 <- 
  network_template(nbhd_value = 2,
                   x_values = c(300, -300, rep(0, 4), -1600, -300, 0, 0, 0, 500,
                                -300),
                   y_values = c(400, 0, -300, rep(0, 7), 300, 0, -1500),
                   col_scale = 1,
                   bbox = c(-0.02, 1.02, -0.02, 1.02),
                   title = "Figure 7. Central Bronx bike sharing")

ggsave("output/report-2/figure_2_07.png", plot = figure_2_07, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 8. Crown Heights/Brownsville

figure_2_08 <- 
  network_template(nbhd_value = 3, 
                   x_values = c(0, -300, 0, 0, 0, 300, -300, 0, 0, 0, 0, 300, 0,
                                0), 
                   y_values = 0,
                   col_scale = 0.5,
                   bbox = c(0, 1, -0.05, 1.05),
                   title = "Figure 8. Crown Heights/Brownsville bike sharing")

ggsave("output/report-2/figure_2_08.png", plot = figure_2_08, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 9. East Bronx
## Add missing water

figure_2_09 <- 
  network_template(nbhd_value = 4, 
                   x_values = c(rep(0, 7), -2500, 0, 800, rep(0, 9)),
                   y_values = c(rep(0, 6), -500, 0, -600, 700, rep(0, 9)),
                   col_scale = 1,
                   bbox = c(-0.15, 1.15, 0, 1),
                   title = "Figure 9. East Bronx bike sharing")

ggsave("output/report-2/figure_2_09.png", plot = figure_2_09, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 10. East New York/Canarsie

figure_2_10 <- 
  network_template(nbhd_value = 5,
                   x_values = c(rep(0, 4), 300, rep(0, 6), 1000, rep(0, 6)),
                   y_values = c(rep(0, 4), -200, 0, 0, 400, 0, 500, rep(0, 8)),
                   col_scale = 1,
                   bbox = c(0, 1, -0.03, 1.03),
                   title = "Figure 10. East New York/Canarsie bike sharing")

ggsave("output/report-2/figure_2_10.png", plot = figure_2_10, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 11. Far Rockaway
## Need to figure out water

figure_2_11 <- 
  network_template(nbhd_value = 6,
                   x_values = c(0, -200, 0, 200, -400, 0),
                   y_values = c(-800, 1300, 0, -600, -100, 100),
                   col_scale = 1,
                   bbox = c(-0.1, 1.1, -0.2, 1.3),
                   title = "Figure 11. Far Rockaway bike sharing")

ggsave("output/report-2/figure_2_11.png", plot = figure_2_11, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 12. Jackson Heights/Flushing

figure_2_12 <- 
  network_template(nbhd_value = 7,
                   x_values = 0,
                   y_values = c(350, -300, 0, 0, 0, 0, 200),
                   col_scale = 1,
                   bbox = c(0, 1, -0.05, 1.05),
                   title = "Figure 4. Jackson Heights/Flushing bike sharing")

ggsave("output/report-2/figure_2_12.png", plot = figure_2_12, width = 8,
       height = 6, units = "in", dpi = 300)


## Figure 13. Jamaica

figure_2_13 <- 
  network_template(nbhd_value = 8,
                   x_values = 0,
                   y_values = 0,
                   col_scale = 1,
                   bbox = c(-0.1, 1.1, -0.18, 1.18),
                   title = "Figure 13. Jamaica bike sharing")

ggsave("output/report-2/figure_2_13.png", plot = figure_2_13, width = 8,
       height = 8, units = "in", dpi = 300)


##  Figure 14. South Bronx

figure_2_14 <- 
  network_template(nbhd_value = 9, 
                   x_values = 0, 
                   y_values = 0, 
                   col_scale = 1.3,
                   bbox = c(-0.13, 1.13, 0, 1),
                   title = "Figure 14. South Bronx bike sharing")

figure_2_14$layers[[13]] <-
  geom_text_repel(data = filter(osm_networks[[9]][[3]], 
                                !(stop_id %in% c("414", "415"))), 
                  mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                label = stop_name),
                  nudge_x = c(0, -500, -450, 0, -250, 0, -350, -350, 0, 700,
                              650, 450, 950, 0, 0, -350, 350, 350, 0),
                  nudge_y = c(200, 200, 200, -200, -200, 250, 0, 0, 100, 0, 0,
                              0, 0, -150, 0, -200, 0, 0, -150),
                  family = "Futura-Medium",
                  segment.size = .4,
                  min.segment.length = 0,
                  point.padding = 1)

ggsave("output/report-2/figure_2_14.png", plot = figure_2_14, width = 8, 
       height = 8, units = "in", dpi = 300)


## Figure 15. Sunset Park/Bay Ridge
## NEED TO FIX WATER

figure_2_15 <- 
  network_template(nbhd_value = 10,
                   x_values = c(-200, 200),
                   y_values = 0,
                   col_scale = 1,
                   bbox = c(-0.2, 1.2, -0.1, 1.1),
                   title = "Figure 15. Sunset Park/Bay Ridge bike sharing")

ggsave("output/report-2/figure_2_15.png", plot = figure_2_15, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 16. Upper Manhattan
## NEED TO SWAP OUT NYC WATER WITH MSA WATER


figure_2_16 <- 
  network_template(nbhd_value = 11,
                   x_values = c(rep(0, 9), 600, 0, 300, rep(0, 11)),
                   y_values = c(rep(0, 9), -300, rep(0, 11), -300, 0),
                   col_scale = 1,
                   bbox = c(-.2, 1.3, 0, 1),
                   title = "Figure 16. Upper Manhattan bike sharing")

ggsave("output/report-2/figure_2_16.png", plot = figure_2_16, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 17. West Bronx
## NEED TO SWAP OUT NYC WATER WITH MSA WATER


figure_2_17 <- 
  network_template(nbhd_value = 12,
                   x_values = c(rep(0, 6), 300, 0, 0, 0, -500),
                   y_values = c(200, 0, 200, 200, rep(0, 7)),
                   col_scale = 1,
                   bbox = c(-0.2, 1.2, -0.01, 1.01),
                   title = "Figure 17. West Bronx bike sharing")

ggsave("output/report-2/figure_2_17.png", plot = figure_2_17, width = 8,
       height = 8, units = "in", dpi = 300)






## Report 1 test map

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
