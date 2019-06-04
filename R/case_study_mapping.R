##Case studies report mapping

# Figure 1. Expansion areas

figure[[2.1]] <- 
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

tmap_save(figure[[2.1]], "output/figure_2.1.png", width = 2400, height = 2400)


# Figure 2. Vulnerability of proposed bike sharing expansion areas

figure[[2]] <- 
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

tmap_save(figure[[2.2]], "output/figure_2.2.png", width = 2400, height = 2400)


# Figure 3. Subway accessibility of proposed bike sharing expansion areas

figure[[2.3]] <- 
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

tmap_save(figure[[2.3]], "output/figure_2.3.png", width = 2400, height = 2400)



######### Figure 4. Jackson Heights case study map


## Get Jackson Heights streets and subway stations

jhf_osm <- 
  target_neighbourhoods %>% 
  filter(nbhd == "Jackson Heights/Flushing") %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  bb(ext = 1.2) %>% 
  as.vector() %>%
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()


jhf_streets <- 
  rbind(jhf_osm$osm_polygons %>% st_cast("LINESTRING"), jhf_osm$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

jhf_parks <-
  target_neighbourhoods %>%
  filter(nbhd == "Jackson Heights/Flushing") %>% 
  st_buffer(dist = 2400) %>%
  st_transform(4326) %>%
  st_bbox() %>%
  bb(ext = 1.2) %>%
  as.vector() %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

jhf_parks <- 
  jhf_parks[[6]] %>%
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

jhf_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "Jackson Heights/Flushing"))) > 0,]

figure[[2.4]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(target_neighbourhoods[7,]), ext = 1.1)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_water) +
  tm_fill(col = "#a6cee3", alpha = 0.5)+
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(jhf_parks) + 
  tm_fill(col = "#a6d854", alpha = 0.5) +
  tm_shape(jhf[[2]]%>% st_buffer(dist = 30) %>% st_erase(subway_total_catchment)) +
  tm_fill(col = "#fc8d62", alpha = 0.5) +
  tm_shape(jhf[[3]]%>% st_buffer(dist = 30) %>% st_erase(jhf[[4]]) ) +
  tm_fill(col= "#e5c494", alpha = 0.5) +
  tm_shape(jhf[[2]]%>% st_buffer(dist = 30)%>%st_intersection(subway_total_catchment)) +
  tm_fill(col= "##8da0cb", alpha =0.5) +
  tm_shape(jhf_streets %>% filter(!is.na(name))) +
  tm_lines(col = "grey50", alpha = 0.5) +
  tm_shape(subway_lines) +
  tm_lines(col = "grey35", lwd = 3) +
  tm_shape(jhf_stations) +
  tm_dots(col = "grey35", size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "black") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "#e5c494", 
                alpha = 0.3, border.alpha = 0.1, border.col = "blue") +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#fc8d62", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#8da0cb", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 4. Jackson Heights/Flushing case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure[[2.4]], "output/figure_2.4.png", width = 2400)
plot(figure[[2.4]])

# Figure 5. South Bronx case study map

## Get Jackson Heights streets and subway stations

sbronx_osm <- 
  target_neighbourhoods %>% 
  filter(nbhd == "South Bronx") %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  bb(ext = 1.2) %>% 
  as.vector() %>%
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()


sbronx_streets <- 
  rbind(sbronx_osm$osm_polygons %>% st_cast("LINESTRING"), sbronx_osm$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

sbronx_parks <-
  target_neighbourhoods %>%
  filter(nbhd == "South Bronx") %>% 
  st_buffer(dist = 2400) %>%
  st_transform(4326) %>%
  st_bbox() %>%
  bb(ext = 1.2) %>%
  as.vector() %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

sbronx_parks <- 
  sbronx_parks[[6]] %>%
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

sbronx_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "South Bronx"))) > 0,]

figure[[2.5]] <- 
  tm_shape(nyc_msa, bbox = bb(st_bbox(target_neighbourhoods[9,]), ext = 1.1)) +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_shape(jhf_parks) + 
  tm_fill(col = "grey90") +
  tm_shape(sbronx_streets %>% filter(!is.na(name))) +
  tm_lines(col = "grey50") +
  tm_shape(sbronx[[2]]%>%st_erase(subway_total_catchment)%>% st_buffer(dist=30)) +
  tm_fill(col = "#b3de69", alpha = 0.3) +
  tm_shape(sbronx[[3]]%>%st_erase(sbronx[[4]])%>% st_buffer(dist=30)) +
  tm_fill(col= "#fb8072", alpha = 0.3) +
  tm_shape(sbronx[[2]]%>%st_intersection(subway_total_catchment)%>% st_buffer(dist = 30)) +
  tm_fill(col= "#8dd3c7", alpha = 0.3) +
  tm_shape(subway_lines) +
  tm_lines(lwd = 2, alpha = 0.1) +
  tm_shape(sbronx_stations) +
  tm_dots(size = 0.5) +
  tm_text("stop_name", size = 0.7, 
          xmod = c(  0, -1.3,   0,    1,   0,  0.6, -0.4),
          ymod = c(0.8,  0.8, 0.8, -0.8, 0.8, -0.8,  0.8)) +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "black") +
  tm_add_legend(type = "fill", labels = "Access to subway", col = "#fb8072", 
                alpha = 0.3, border.alpha = 0.1, border.col = "blue") +
  tm_add_legend(type = "fill", labels = "Access to bike sharing", col = "#b3de69", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_add_legend(type = "fill", labels = "Access to both", col = "#8dd3c7", 
                alpha = 0.3, border.alpha = 0.1, border.col = ) +
  tm_layout(main.title = "Figure 5. South Bronx case study",
            frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            main.title.fontfamily = "Futura-CondensedExtraBold")

tmap_save(figure[[2.5]], "output/figure_2.5.png", height = 2400)
