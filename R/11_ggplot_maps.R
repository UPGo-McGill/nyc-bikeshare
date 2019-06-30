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


## Network map template

network_template <- function(nbhd_value) {
  
  network_geom <- 
    st_sf(service = factor(c("Access to subway", "Access to bike sharing", 
                             "Access to both"),
                           levels = c("Access to subway",
                                      "Access to bike sharing",
                                      "Access to both")), 
          geometry = c(networks[[nbhd_value]][[2]], networks[[nbhd_value]][[1]],
                       networks[[nbhd_value]][[3]]))
  
  ggplot() +
    geom_sf(data = nyc_msa, fill = "#F0F0F0", lwd = 0) +
    geom_sf(data = nyc_city, colour = "grey92") +
    geom_sf(data = osm_networks[[nbhd_value]][[1]], fill = "#BDD490", 
            lwd = 0) +
    geom_sf(data = network_geom, mapping = aes(fill = service), lwd = 0) +
    geom_sf(data = nyc_water, fill = "#E5F7FF",
            colour = alpha("#326CAB", 0.15)) +
    geom_sf(data = subway_lines, colour = alpha("grey50", 0.7), lwd = 2) +
    geom_sf(data = osm_networks[[nbhd_value]][[2]],
            colour = alpha("black", 0.12)) +
    geom_sf(data = osm_networks[[nbhd_value]][[3]], colour = "grey50",
            size = 5) +
    annotation_scale(location = "br", width_hint = 0.4, line_col = "grey50",
                     bar_cols = c("grey50", "white"), 
                     unit_category = "imperial", style = "ticks") +
    scale_fill_manual(name = NULL, values = c("grey75", "#F8BCA5", "#E98463")) +
    guides(fill = guide_legend(override.aes = list(colour = "black", 
                                                   lwd = 0.5))) +
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
                    values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                               "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
                               "#cab2d6", "#6a3d9a", "#ffff99", "#b15928")) +
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
  network_template(7) +
  geom_text_repel(data = osm_networks[[7]][[3]], 
                  mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                label = stop_name),
                  nudge_x = c(0,      0, 0, 0, 0, 0, 0),
                  nudge_y = c(350, -300, 0, 0, 0, 0, 200),
                  family = "Futura-Medium",
                  segment.size = .4,
                  min.segment.length = 0,
                  point.padding = 1) +
  ggtitle("Figure 4. Jackson Heights/Flushing bike sharing") +
  gg_bbox(networks[[7]][[1]], 0, 1, -0.02, 1.02)

ggsave("output/report-2/figure_2_04.png", plot = figure_2_04, width = 8,
       height = 6, units = "in", dpi = 300)


##  Figure 5. South Bronx

figure_2_05 <- 
  network_template(9) +
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
                  point.padding = 1) +
  ggtitle("Figure 5. South Bronx bike sharing") +
  gg_bbox(networks[[9]][[1]], -0.1, 1.1, 0, 1)

ggsave("output/report-2/figure_2_05.png", plot = figure_2_05, width = 8, 
       height = 8, units = "in", dpi = 300)


## Figure 6. Bushwick/Ridgewood

figure_2_06 <- 
  network_template(1) +
  geom_text_repel(data = osm_networks[[1]][[3]], 
                  mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                label = stop_name),
                  family = "Futura-Medium",
                  segment.size = .4,
                  min.segment.length = 0,
                  point.padding = 1) +
  ggtitle("Figure 6. Bushwick/Ridgewood bike sharing") +
  gg_bbox(networks[[1]][[1]], -0.05, 1.05, -0.1, 1.1)

ggsave("output/report-2/figure_2_06.png", plot = figure_2_06, width = 8,
       height = 8, units = "in", dpi = 300)


## Figure 7. Central Bronx

figure_2_07 <- 
  network_template(2) +
  geom_text_repel(data = osm_networks[[2]][[3]],
                  mapping = aes(x = map_dbl(geometry, ~st_coordinates(.)[1]),
                                y = map_dbl(geometry, ~st_coordinates(.)[2]),
                                label = stop_name),
                  nudge_x = c(0, 0, 0, -300, 0, 0, 0, -300, 0, 0, 0, 0, 0),
                  nudge_y = c(0, 0, 0,    0, 0, 0, 0,    0, 0, 0, 0, 0, 0),
                  family = "Futura-Medium",
                  segment.size = .4,
                  min.segment.length = 0,
                  point.padding = 1) +
  ggtitle("Figure 7. Central Bronx bike sharing") +
  gg_bbox(networks[[2]][[1]])

ggsave("output/report-2/figure_2_07.png", plot = figure_2_07, width = 8,
       height = 8, units = "in", dpi = 300)







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
