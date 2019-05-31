### CASE STUDY EXTRAS ###########

## Create simple city DF to use for catchment calculations

network_city <-
  as(tibble(data = 1, geometry = nyc_city) %>% st_as_sf(), "Spatial")


##Create subway station data with coordinates to create subway access network
subway_stations_coordinates <-
  st_read("data", "stops_nyc_subway_nov2018", stringsAsFactors = FALSE) %>%  
  st_transform(26918) %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 26918) 

##create vulnerable station file with target neighbourhoods and coordinates to create bike service network by neighbourhood
nbhd_subway_stations  <-subway_stations %>%  
  filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
  st_intersection(target_neighbourhoods) %>% 
  st_transform(4326)

write.csv(nbhd_subway_stations, "data/nbhd_subway_stations.csv")


##get subway access network

subway_network <- 
  st_read("data/subway_network/edges.shp", stringsAsFactors = FALSE) %>%  
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918) %>%
  st_union()

write.csv(subway_network, "data/subway_network.csv")


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


## Develop JHF network catchment areas
jhf_bike_network <- 
  st_read("data/neighbourhoods/Jackson_Heights_Flushing/edges", stringsAsFactors = FALSE) %>%  
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918) %>%
  st_union()


## Versions of catchment areas to use for service population estimates

jhf_bike_catchment_population <- 
  calc_network_catchment(
    jhf_network, network_city, as(jhf_stations, "Spatial"), c("data"),
    distance = 100, maximpedance = 2400, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()

jhf_subway_catchment_population <- 
  calc_network_catchment(
    jhf_network, network_city, as(jhf_stations, "Spatial"), c("data"),
    distance = 100, maximpedance = 960, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()


## Service population estimates

jhf_comparison <-
  tibble(service = c("both", "bike", "subway"),
         geometry = c(jhf_bike_catchment_population, st_erase(
           jhf_bike_catchment_population, jhf_subway_catchment_population),
           jhf_subway_catchment_population)) %>% 
  st_as_sf() %>% 
  st_set_crs(26918) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))



## Get South Bronx streets and subway stations

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


sbronx_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "South Bronx"))) > 0,]


## Develop Bronx network catchment areas
sbronx_bike_network <- 
  st_read("data/neighbourhoods/South_Bronx/edges", stringsAsFactors = FALSE) %>%  
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918)
