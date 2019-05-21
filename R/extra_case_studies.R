### CASE STUDY EXTRAS ###########

## Get Jackson Heights streets

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

jhf_dodgr <- 
  target_neighbourhoods %>% 
  filter(nbhd == "Jackson Heights/Flushing") %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  bb(ext = 1.2) %>% 
  as.vector() %>%
  dodgr_streetnet(quiet = FALSE)



### DODGR TRY

queens_dodgr <- dodgr_streetnet("queens new york city")
queens_2 <- weight_streetnet(queens_dodgr, wt_profile = 1) %>% 
  dodgr::dodgr_to_sf()
queens_3 <- st_sf(queens_2) %>% as_tibble() %>% st_as_sf()

queens_3 <- queens_3 %>% st_transform(26918)

queens_clipped <- 
  target_neighbourhoods %>% 
  filter(nbhd == "Jackson Heights/Flushing") %>% 
  st_buffer(2500) %>%
  st_intersection(queens_3, .)

queens_network <- 
  queens_clipped %>% 
  as("Spatial") %>% 
  SpatialLinesNetwork()

queens_catchment <- 
  calc_network_catchment(
    queens_network,
    network_city,
    as(jhf_stations, "Spatial"),
    c("data"),
    distance = 100,
    maximpedance = 2400,
    dissolve = TRUE)

jhf_buffer <- 
  queens_catchment %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()






jhf_streets <- 
  rbind(jhf_osm$osm_polygons %>% st_cast("LINESTRING"), jhf_osm$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)


## Develop network catchment areas

network_city <-
  as(tibble(data = 1, geometry = nyc_city) %>% st_as_sf(), "Spatial")

jhf_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "Jackson Heights/Flushing"))) > 0,]

jhf_network <- 
  jhf_streets %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>% 
  as("Spatial") %>% 
  SpatialLinesNetwork(tolerance = 100)

jhf_catchment <- 
  calc_network_catchment(
    jhf_network,
    network_city,
    as(jhf_stations, "Spatial"),
    c("data"),
    distance = 25,
    maximpedance = 2000,
    dissolve = TRUE)

jhf_buffer <- 
  jhf_catchment %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union() %>% 
  fill_holes(1000)


















## Network catchment testing

test_CTs <- as(CTs,"Spatial")
test_city <- as(tibble(data = 1, geometry = nyc_city) %>% st_as_sf(), "Spatial")

test_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods[7,])) > 0,]

test_target <-as(subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods[7,])) > 0,], "Spatial")

test_network <- 
  jackson_heights_streets$osm_lines %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>% 
  as("Spatial") %>% 
  SpatialLinesNetwork(tolerance = 100)

test_catchment <- 
  calc_network_catchment(
    test_network,
    test_city,
    test_target,
    c("data"),
    distance = 150,
    maximpedance = 2000,
    dissolve = TRUE)

test_buffer <- 
  test_catchment %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union() %>% 
  fill_holes(1000)


















test_streets <- 
  jackson_heights_streets$osm_lines %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  filter(!is.na(name)) %>% 
  select(osm_id, name, geometry)

plot(test_network, component = "graph")


test_catchment %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>%
  st_combine %>% 
  st_union %>% 
  st_union %>% 
  fill_holes(1000) %>% 
  plot(col = "red")


test_list <- split(test_stations, test_stations$stop_id)


test_map <- map(test_list, ~{
  test_target <- as(., "Spatial")
  
  test_catchment <- 
    calc_network_catchment(
      test_network,
      test_CTs,
      test_target,
      c("pop_total"),
      distance = 1000,
      maximpedance = 2000,
      dissolve = TRUE)
  
  test_buffer <- 
    test_catchment %>% 
    st_as_sf() %>% 
    as_tibble() %>% 
    st_as_sf()
  
  test_buffer
})


test_map <- map(test_map, ~st_union(st_combine(.)))

reduce(test_map, st_combine)

st_union(test_map[[1]], test_map[[]])

plot(test_map[[6]])
















