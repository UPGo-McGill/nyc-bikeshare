### Extract network buffers and assemble mapping components ####################

## Code to rebuild networks

#osm_networks <- suppressMessages(
#  map(target_neighbourhoods$nbhd, map_creator) %>% 
#    set_names(target_neighbourhoods$nbhd))

#osmnx <- import("osmnx")
#networkx <- import("networkx")

#pb <- progress_estimated(length(osm_networks))

#bike_networks <- suppressWarnings(
#  map(osm_networks, ~{
#    network <- network_creator(., 2400)
#    pb$tick()$print()
#    network}) %>% 
#    set_names(target_neighbourhoods$nbhd))

#pb <- progress_estimated(length(osm_networks))

#subway_networks <- suppressWarnings(
#  map(osm_networks, ~{
#    network <- network_creator(., 960, extra_subway = TRUE)
#    pb$tick()$print()
#    network}) %>% 
#    set_names(target_neighbourhoods$nbhd))

#rm(pb, osmnx, networkx)


## Import RData files to avoid rebuilding bike share and subway networks

load("data/osm_networks.RData")
load("data/bike_networks.RData")
load("data/subway_networks.RData")

  
## Erase and clip Manhattan

bike_networks[[2]] <- suppressWarnings(
  bike_networks[[2]] %>% 
  st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx"))) %>% 
  st_erase(nyc_water) %>%
  # Extra step to remove spurious network segments
  st_erase(st_buffer(st_centroid(filter(CTs, GEOID == 36005000200)), 1500)))

bike_networks[[4]] <-
  bike_networks[[4]] %>% 
  st_intersection(nyc_city) %>% 
  st_collection_extract("LINESTRING") %>% 
  st_union()

bike_networks[[9]] <- suppressWarnings(
  bike_networks[[9]] %>% 
  st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx"))) %>% 
  st_erase(nyc_water))

bike_networks[[11]] <- 
  bike_networks[[11]] %>% 
  st_erase(filter(clusters, nbhd == "West Bronx")) %>% 
  st_erase(bronx) %>% 
  st_intersection(nyc_city) %>% 
  st_collection_extract("LINESTRING") %>% 
  st_union()

bike_networks[[12]] <- suppressWarnings(
  bike_networks[[12]] %>% 
  st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx"))) %>% 
  st_erase(nyc_water))


## Polygonize and buffer appropriately

pb <- progress_estimated(length(bike_networks) * 5)

networks <- 
  map2(bike_networks, subway_networks, ~{
    
    bike <- .x %>% 
      st_cast("LINESTRING") %>%
      st_buffer(20) %>% 
      st_union()
    pb$tick()$print()
    
    subway <- .y %>% 
      st_cast("LINESTRING") %>%
      st_buffer(20) %>% 
      st_union()
    pb$tick()$print()
    
    bike_and_subway <- st_intersection(bike, subway)
    pb$tick()$print()
    
    bike_polygon <- st_buffer(st_polygonize(.x), 50)
    pb$tick()$print()
    
    subway_polygon <- st_buffer(st_polygonize(.y), 50)
    pb$tick()$print()
    
    list(bike, subway, bike_and_subway, bike_polygon, subway_polygon) %>% 
      set_names("bike", "subway", "bike_and_subway", "bike_polygon", 
                "subway_polygon")
  }) %>% 
  set_names(target_neighbourhoods$nbhd)

rm(pb)

save(networks, file = "data/networks.RData")
