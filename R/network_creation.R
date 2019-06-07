### Extract network buffers and assemble mapping components ####################

# Initialize Python packages

osmnx <- import("osmnx")
networkx <- import("networkx")


## Generate osm networks

osm_networks <- 
  map(target_neighbourhoods$nbhd, map_creator) %>% 
  set_names(target_neighbourhoods$nbhd)


## Import RData files to avoid rebuilding bike share and subway networks

load("data/bike_share_networks.RData")
load("data/subway_networks.RData")


## Code to rebuild bike share and subway networks

#bike_share_networks <-
#  map(osm_networks, network_creator, 2400) %>% 
#  set_names(target_neighbourhoods$nbhd)

#subway_networks <-
#  map(osm_networks, network_creator, 960, extra_subway = TRUE) %>% 
#  set_names(target_neighbourhoods$nbhd)

