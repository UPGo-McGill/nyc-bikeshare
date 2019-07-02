### HELPER FUNCTIONS

## Load libraries and fonts

library(osmdata)
library(smoothr)
library(tidycensus)
library(tigris)
library(tmap)
library(tmaptools)
library(units)
library(grid)
library(gridExtra)
library(scales)
library(reticulate)
library(ggpubr)
library(tidyverse)
library(sf)
library(extrafont)
library(lwgeom)
library(ggspatial)
library(viridis)
library(ggrepel)

options(tigris_use_cache = TRUE)
suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))


## Set buffer values

bike_distance <- 300
subway_distance <- 800
expansion_distance <- 2000


## st_erase helper function

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))


## st_intersect_summarize helper function

st_intersect_summarize <- function(data, poly, group_vars, population, sum_vars,
                                   mean_vars) {
  
  pop <- enquo(population)
  
  data <- data %>% 
    mutate(CT_area = st_area(.))
  
  intersects <- suppressWarnings(st_intersection(data, poly)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = !! pop * int_area_pct) %>%
    group_by(!!! group_vars)
  
  population <- intersects %>% 
    summarize(!! pop := sum(population_int, na.rm = TRUE))
  
  sums <- intersects %>%
    summarize_at(
      sum_vars,
      ~{sum(. * int_area_pct, na.rm = TRUE) / sum(population_int,
                                                  na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join)) %>% 
    drop_units()
  
}


## index_create helper function

index_create <- function(var) {
  (var - min(var)) / (max(var) - min(var))
}


## service_create helper function

service_create <- function(year, distance) {
  suppressWarnings(bike_stations %>%
                     filter(Year == year) %>%
                     st_buffer(distance) %>%
                     st_union() %>%
                     st_erase(nyc_water))
}


## Map and find demographics for bike and subway access by neighbourhood

network_calculator <- function(bike_path, subway_path, man_erase = FALSE,
                               man_clip = FALSE) {
  
  bike_network <-
    st_read(bike_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union() %>%
    st_erase(nyc_water)
  
  if (man_erase) {
    bike_network <- bike_network %>% 
      st_erase(manhattan %>% st_erase(filter(clusters, nbhd == "West Bronx")))
  }
  
  if (man_clip) {
    bike_network <- bike_network %>% 
      st_erase(filter(clusters, nbhd == "West Bronx")) %>% 
      st_erase(bronx) 
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
  
  if (man_clip) {
    bike_catchment <- bike_catchment %>% 
      st_cast("POLYGON") %>% 
      as_tibble() %>% 
      st_as_sf() %>% 
      filter(st_area(.) > set_units(100000, m^2)) %>% 
      st_union()
  }
  
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
      mean_vars = vars(med_income, vulnerability_index)) %>%
    st_drop_geometry()
  
  list(comparison, bike_network, subway_network, bike_catchment)
}


map_creator <- function(nbhd_name) {
  
  # Get osm layer for line segments and maps
  
  nbhd_osm <- 
    target_neighbourhoods %>% 
    filter(nbhd == nbhd_name) %>% 
    st_transform(4326) %>% 
    st_bbox() %>% 
    bb(ext = 2.5) %>% 
    as.vector() %>%
    opq() %>% 
    add_osm_feature(key = "highway") %>% 
    osmdata_sf()
  
  osm_points <- nbhd_osm$osm_points %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    select(osm_id, geometry)
  
  osm_streets <- 
    rbind(nbhd_osm$osm_polygons %>% st_cast("LINESTRING"),
          nbhd_osm$osm_lines) %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    #st_transform(26918) %>%
    select(osm_id, name, geometry)
  
  osm_parks <-
    target_neighbourhoods %>%
    filter(nbhd == nbhd_name) %>% 
    st_transform(4326) %>%
    st_bbox() %>%
    bb(ext = 2.5) %>%
    as.vector() %>%
    opq() %>%
    add_osm_feature(key = "leisure", value = "park") %>%
    osmdata_sf() %>% 
    `$`(osm_polygons) %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    st_transform(26918) %>%
    select(osm_id, name, geometry)
  
  nbhd_stations <- 
    subway_stations_vulnerability %>% 
    st_join(filter(target_neighbourhoods, nbhd == nbhd_name), left = FALSE) %>% 
    select(-vulnerability_index, - pop_no_subway)
  
  list(osm_parks, osm_streets, nbhd_stations, osm_points) %>% 
    set_names("parks", "streets", "stations", "nodes") %>%
    map(st_transform, 26918)
}


network_creator <- function(nbhd_list, network_dist, extra_subway = FALSE) {
  
  # Get graph in Python and convert to GDF
  if (extra_subway) {
    graph <-
      nbhd_list$stations %>% 
      st_buffer(4000) %>% 
      st_union() %>% 
      st_intersection(subway_stations, .) %>%
      st_join(target_neighbourhoods) %>% 
      select(-vulnerability_index, -pop_no_subway) %>%
      st_transform(4326) %>%
      st_geometry() %>%
      map(~{
        osmnx$graph_from_point(
          c(st_coordinates(.)[2], st_coordinates(.)[1]), distance = network_dist,
          distance_type = "network", network_type = "all", simplify = FALSE)
      })
  } else {
    graph <- 
      nbhd_list$stations %>% 
      st_transform(4326) %>%
      st_geometry() %>%
      map(~{
        osmnx$graph_from_point(
          c(st_coordinates(.)[2], st_coordinates(.)[1]), distance = network_dist,
          distance_type = "network", network_type = "all", simplify = FALSE)
        })}
  
  graph <- networkx$compose_all(graph)
  
  graph_edges <- 
    osmnx$graph_to_gdfs(graph, nodes = FALSE, edges = TRUE) %>%
    as_tibble() %>% 
    select(osmid, u, v)
  
  # Connect GDF to osm layer
  graph_lines <- filter(nbhd_list$streets, osm_id %in% graph_edges$osmid)
  
  # Rebuild line segments
  edges <- 
    map(graph_lines$osm_id,
        function(x) filter(graph_edges, osmid %in% x)) %>%
    bind_rows()
  
  points <- filter(nbhd_list$nodes, osm_id %in% c(edges$u, edges$v))
  
  final_edges <- st_split(st_union(graph_lines), points) %>%
    st_collection_extract("LINESTRING")
  
  network <- final_edges[lengths(st_intersects(final_edges, points)) == 2] %>% 
    st_union()
  
  network %>% 
    st_transform(26918)
}


## gg_bbox helper function

gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1) {
  
  bbox <- st_bbox(geom)
  
  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)
  
  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)
  
  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y))
}