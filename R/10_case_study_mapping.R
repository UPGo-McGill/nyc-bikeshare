##Case studies report mapping


#########  Figure 1. Expansion areas

figure_2_01 <- 
  base_map +
  tm_shape(target_neighbourhoods) +
  tm_fill(col = "nbhd", title = "",
          palette = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                      "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
                      "#ffff99", "#b15928")) +
  tm_borders(col = "white", lwd = 2) +
  tm_shape(bike_service_areas_no_holes$geometry[3]) +
  tm_fill(col = "grey40") +
  tm_shape(subway_lines) +
  tm_lines(col = "grey90", alpha = 0.75) +
  tm_layout(title = "Figure 1. Proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "Existing Citi Bike service area",
                col = "grey40", border.lwd = 0)

tmap_save(figure_2_01, "output/figure_2_01.png", width = 2400, height = 2400)


#########  Figure 2. Vulnerability of proposed bike sharing expansion areas

figure_2_02 <- 
  base_map +
  tm_shape(CTs) +
  tm_fill("vulnerability_index", palette = "-RdYlGn", border.alpha = 0,
          title = "", n = 8) +
  tm_shape(st_erase(nyc_city, st_union(target_neighbourhoods))) +
  tm_fill(col = "grey80", alpha = 0.6) +
  tm_shape(target_neighbourhoods) +
  tm_borders(col = "white", lwd = 2) +
  tm_layout(title = "Figure 2. Vulnerability of proposed bike sharing expansion areas") +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0")

tmap_save(figure_2_02, "output/figure_2_02.png", width = 2400, height = 2400)



#########  Figure 3. Subway accessibility of proposed bike sharing expansion areas

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

tmap_save(figure_2_03, "output/figure_2_03.png", width = 2400, height = 2400)



######### Figure 4. Jackson Heights case study map


nbhd_value <- 7

figure_2_4 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 4. Jackson Heights/Flushing case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_04, "output/figure_2_04.png", height = 2400)


#########  Figure 5. South Bronx case study map

nbhd_value <- 9

figure_2_05 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.2), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 5. South Bronx case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")


tmap_save(figure_2_05, "output/figure_2_05.png", height = 2400)


#########  Figure 6.  Bushwick / Ridgewood Map

nbhd_value <- 1

figure_2_06 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 6. Bushwick / Ridgewood",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_06, "output/figure_2_06.png", height = 2400)



#########  Figure 7.  Central Bronx Case Study Map

nbhd_value <- 2

figure_2_07 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 7. Central Bronx",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_07, "output/figure_2_07.png", height = 2400)


#########  Figure 8.  Crown Heights / Brownsville Map

nbhd_value <- 3

figure_2_08 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 8. Crown Heights / Brownsville",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_08, "output/figure_2_08.png", height = 2400)



#########  Figure 9. East Bronx Map

nbhd_value <- 4

figure_2_09 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 9. East Bronx",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_09, "output/figure_2_09.png", height = 2400)



#########  Figure 10. East New York / Canarsie Map

nbhd_value <- 5

figure_2_10 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.05), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 10. East New York / Canarsie",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_10, "output/figure_2_10.png", height = 2400)



#########  Figure 11. Far Rockaway Map

nbhd_value <- 6

figure_2_11 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.25), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 11. Far Rockaway",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_11, "output/figure_2_11.png", height = 2400)



#########  Figure 12. Jamaica Map

nbhd_value <- 8

figure_2_12 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.15), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 12. Jamaica",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_12, "output/figure_2_12.png", height = 2400)




#########  Figure 13. Sunset Park / Bay Ridge Map

nbhd_value <- 10

figure_2_12 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.25), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 13. Sunset Park / Bay Ridge",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_13, "output/figure_2_13.png", height = 2400)




######### Figure 14. Upper Manhattan Map

nbhd_value <- 11

figure_2_14 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.2), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 14. Upper Manhattan",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_14, "output/figure_2_14.png", height = 2400)


#########  Figure 15. West Bronx Map

nbhd_value <- 12

figure_2_15 <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(networks[[nbhd_value]][[4]]), ext = 1.1), unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey92", title = "Base Map") +
  tm_shape(osm_networks[[nbhd_value]][[1]]) + 
  tm_fill(col = "#BDD490") +
  tm_shape(networks[[nbhd_value]][[1]]) +
  tm_fill(col = "#F8BCA5") +
  tm_shape(networks[[nbhd_value]][[2]]) +
  tm_fill(col= "grey75") +
  tm_shape(networks[[nbhd_value]][[3]]) +
  tm_fill(col= "#E98463") + 
  tm_shape(nyc_water) +
  tm_fill(col = "#E5F7FF")+
  tm_borders(col = "#326CAB", lwd = 1, alpha = 0.15) + 
  tm_shape(osm_networks[[nbhd_value]][[2]]) +
  tm_lines(col = "black", alpha = 0.12) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3, alpha = 0.7) +
  tm_shape(osm_networks[[nbhd_value]][[3]]) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "grey65", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#FBAC8D", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#E87142", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 15. West Bronx",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure_2_15, "output/figure_2_15.png", height = 2400)
