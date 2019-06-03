### CASE STUDY EXTRAS ###########


##create vulnerable station file with target neighbourhoods and coordinates to create bike service network by neighbourhood in Python
target_subway_stations  <-subway_stations %>%  
  filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
  st_intersection(target_neighbourhoods) %>% 
  st_transform(4326)

write.csv(target_subway_stations, "data/nbhd_subway_stations.csv")


##Create subway station data with coordinates to create subway access network in Python
nbhd_buffer_subway_stations <- subway_stations %>% 
  st_intersection(st_buffer(target_neighbourhoods, dist = 2400)) %>% 
  st_transform(4326)

write.csv(nbhd_buffer_subway_stations, "data/nbhd_buffer_subway_stations.csv")



##create function to map and get demographics for bike and subway access by neighbourhood
network_calculator <- function(bike_path, subway_path, man_erase = FALSE, bronx_erase = FALSE) {
  
  bike_network <-
    st_read(bike_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union()
  
  if (man_erase) {
    bike_network <- bike_network %>% st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx")))
  }
  
  if (bronx_erase) {
    bike_network <- bike_network %>% st_erase(filter(clusters, nbhd == "West Bronx")) %>% st_erase(filter(clusters, nbhd == "West Bronx"))
  }
  
  
  subway_network <-
    st_read(subway_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union()
  
  bike_catchment <-
    bike_network %>%
    st_polygonize() %>%
    st_buffer(dist = 50)
  
  subway_catchment <-
    subway_network %>%
    st_polygonize() %>%
    st_buffer(dist = 50)
  
  comparison <-
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(bike_catchment,
                        st_erase(bike_catchment, subway_catchment))) %>%
    st_as_sf() %>%
    st_set_crs(26918) %>%
    st_intersect_summarize(
      CTs,
      .,
      group_vars = vars(service),
      population = pop_total,
      sum_vars = vars(pop_white, education, poverty),
      mean_vars = vars(med_income, vulnerability_index))
  
  list(comparison, bike_network, subway_network)
}

bushwick <- network_calculator("data/bike_service_network/Bushwick_Ridgewood/edges", "data/subway_service_network/Bushwick_Ridgewood/edges")

cbronx <- network_calculator("data/bike_service_network/Central_Bronx/edges", "data/subway_service_network/Central_Bronx/edges", man_erase = TRUE)

chb <- network_calculator("data/bike_service_network/Crown_Heights_Brownsville/edges", "data/subway_service_network/Crown_Heights_Brownsville/edges")

ebronx <- network_calculator("data/bike_service_network/East_Bronx/edges", "data/subway_service_network/East_Bronx/edges")

enyc <- network_calculator("data/bike_service_network/East_New_York_Canarsie/edges", "data/subway_service_network/East_New_York_Canarsie/edges")

rockaway <- network_calculator("data/bike_service_network/Far_Rockaway/edges", "data/subway_service_network/Far_Rockaway/edges")

jhf <- network_calculator("data/bike_service_network/Jackson_Heights_Flushing/edges", "data/subway_service_network/Jackson_Heights_Flushing/edges")

jamaica <- network_calculator("data/bike_service_network/Jamaica/edges", "data/subway_service_network/Jamaica/edges")

sbronx <- network_calculator("data/bike_service_network/South_Bronx/edges", "data/subway_service_network/South_Bronx/edges", man_erase = TRUE)

spbr <- network_calculator("data/bike_service_network/Sunset_Park_Bay_Ridge/edges", "data/subway_service_network/Sunset_Park_Bay_Ridge/edges")

umanhattan <- network_calculator("data/bike_service_network/Upper_Manhattan/edges", "data/subway_service_network/Upper_Manhattan/edges", bronx_erase = TRUE)

wbronx <- network_calculator("data/bike_service_network/West_Bronx/edges", "data/subway_service_network/West_Bronx/edges", man_erase = TRUE)


View(umanhattan[[1]])

plot(umanhattan[[1]][2,])

bronx_catchment <- wbronx[[2]] %>% st_union(sbronx[[2]]) %>%
  st_polygonize() %>%
  st_buffer(dist = 50)



swbronx_catchment <- wbronx[[1]][2,] %>% st_union(sbronx[[1]][2,]) %>% st_intersect_summarize(
  CTs,
  .,
  group_vars = vars(NA),
  population = pop_total,
  sum_vars = vars(pop_white, education, poverty),
  mean_vars = vars(med_income, vulnerability_index))



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


jhf_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "Jackson Heights/Flushing"))) > 0,]
