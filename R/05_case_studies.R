### CASE STUDY EXTRAS ###########

## Create simple city DF to use for catchment calculations

network_city <-
  as(tibble(data = 1, geometry = nyc_city) %>% st_as_sf(), "Spatial")


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

jhf_streets <- 
  rbind(jhf_osm$osm_polygons %>% st_cast("LINESTRING"), jhf_osm$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

jhf_dodgr <- 
  dodgr_streetnet("queens new york city") %>% 
  weight_streetnet() %>% 
  dodgr_to_sf() %>% 
  st_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918)

jhf_dodgr <- 
  target_neighbourhoods %>% 
  filter(nbhd == "Jackson Heights/Flushing") %>% 
  st_buffer(2500) %>%
  st_intersection(jhf_dodgr, .)


## Develop JHF network catchment areas

jhf_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "Jackson Heights/Flushing"))) > 0,]

jhf_network <- 
  jhf_dodgr %>% 
  as("Spatial") %>% 
  SpatialLinesNetwork()

jhf_bike_catchment <- 
  calc_network_catchment(
    jhf_network, network_city, as(jhf_stations, "Spatial"), c("data"),
    distance = 25, maximpedance = 2400, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()

jhf_subway_catchment <- 
  calc_network_catchment(
    jhf_network, network_city, as(jhf_stations, "Spatial"), c("data"),
    distance = 25, maximpedance = 960, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
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



### REPEAT FOR SOUTH BRONX

## Get South Bronx streets

bronx_osm <- 
  target_neighbourhoods %>% 
  filter(nbhd == "South Bronx") %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  bb(ext = 1.2) %>% 
  as.vector() %>%
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

bronx_streets <- 
  rbind(bronx_osm$osm_polygons %>% st_cast("LINESTRING"),
        bronx_osm$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>%
  select(osm_id, name, geometry)

bronx_dodgr <- 
  dodgr_streetnet("bronx new york city") %>% 
  weight_streetnet() %>% 
  dodgr_to_sf() %>% 
  st_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(26918)

bronx_dodgr <- 
  target_neighbourhoods %>% 
  filter(nbhd == "South Bronx") %>% 
  st_buffer(2500) %>%
  st_intersection(bronx_dodgr, .)


## Develop South Bronx network catchment areas

bronx_stations <- subway_stations_vulnerability[
  lengths(st_intersects(subway_stations_vulnerability,
                        target_neighbourhoods %>% 
                          filter(nbhd == "South Bronx"))) > 0,]

bronx_network <- 
  bronx_dodgr %>% 
  as("Spatial") %>% 
  SpatialLinesNetwork()

bronx_bike_catchment <- 
  calc_network_catchment(
    bronx_network, network_city, as(bronx_stations, "Spatial"), c("data"),
    distance = 25, maximpedance = 2400, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()

bronx_subway_catchment <- 
  calc_network_catchment(
    bronx_network, network_city, as(bronx_stations, "Spatial"), c("data"),
    distance = 25, maximpedance = 960, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()


## Versions of catchment areas to use for service population estimates

bronx_bike_catchment_population <- 
  calc_network_catchment(
    bronx_network, network_city, as(bronx_stations, "Spatial"), c("data"),
    distance = 100, maximpedance = 2400, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()

bronx_subway_catchment_population <- 
  calc_network_catchment(
    bronx_network, network_city, as(bronx_stations, "Spatial"), c("data"),
    distance = 100, maximpedance = 960, dissolve = TRUE) %>% 
  st_as_sf() %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_union()


## Service population estimates

bronx_comparison <-
  tibble(service = c("both", "bike", "subway"),
         geometry = c(bronx_bike_catchment_population, st_erase(
           bronx_bike_catchment_population, bronx_subway_catchment_population),
           bronx_subway_catchment_population)) %>% 
  st_as_sf() %>% 
  st_set_crs(26918) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, immigrant, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))



