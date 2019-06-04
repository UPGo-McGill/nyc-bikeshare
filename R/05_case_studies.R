### CASE STUDY EXTRAS ###########


##create file with subway stations identified as vulnerable by neighbourhood to create bike service network by neighbourhood in Python
bike_service_stationlist  <-subway_stations %>%  
  filter(stop_id %in% subway_buffer_vulnerability$stop_id) %>%
  st_intersection(target_neighbourhoods) %>% 
  st_transform(4326)

write.csv(bike_service_stationlist, "data/bike_service_stationlist.csv")


##Create file with subway station data by neighbourhood to create subway access network in Python
subway_service_stationlist <- subway_stations %>% 
  st_intersection(st_buffer(target_neighbourhoods, dist = 2400)) %>% 
  st_transform(4326)

write.csv(subway_service_stationlist, "data/subway_service_stationlist.csv")


##Create file with coordinates of all subway stations to create total subway access network in Python
total_stationlist <- subway_stations %>% 
  st_transform(4326)

write.csv(total_stationlist, "data/total_stationlist.csv")


py_discover_config()
use_python("C:/Users/hanna/AppData/Local/Programs/Python/Python37", required = T)
py_config()
source_python("data/subway_network.py")
subway_service_network <- neighbourhoodsSmall("subway_service_stationlist.csv")
bike_service_network <- neighborhoodsLarge("bike_service_stationlist.csv")
wholecity <- cityNetwork("total_stationlist.csv")



##create total subway service network for all of NYC
subway_total_catchment <-
  st_read("data/whole_city/edges.shp", stringsAsFactors = FALSE) %>%
  st_transform(26918) %>%
  st_union() %>%
  st_polygonize() %>%
  st_buffer(dist = 50)




##create function to map and find demographics for bike and subway access by neighbourhood
network_calculator <- function(bike_path, subway_path, man_erase = FALSE, man_clip = FALSE) {
  
  bike_network <-
    st_read(bike_path, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    st_as_sf() %>%
    st_transform(26918) %>%
    st_union()
  
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
      mean_vars = vars(med_income, vulnerability_index))
  
  list(comparison, bike_network, subway_network, bike_catchment)
}

##create datasets for each neighbourhood with [[1]] demographic statistics; [[2]] bike access road network; [[3]] subway access road network; [[4]] bike catchment polygon for mapping

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

umanhattan <- network_calculator("data/bike_service_network/Upper_Manhattan/edges", "data/subway_service_network/Upper_Manhattan/edges", man_clip = TRUE)

wbronx <- network_calculator("data/bike_service_network/West_Bronx/edges", "data/subway_service_network/West_Bronx/edges", man_erase = TRUE)



####### Find demographic statistics for muliptle neighbourhoods ######################

swbronx_catchment <- wbronx[[1]][2,] %>% st_union(sbronx[[1]][2,]) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(NA),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


##Find summary demographics for leading potential expansion areas based on vulnerability index
vulnerability_catchment <- cbronx[[1]][2,"geometry"] %>% 
  st_union(chb[[1]][2,"geometry"]) %>%
  st_union(ebronx[[1]][2,"geometry"]) %>% 
  st_union(sbronx[[1]][2,"geometry"]) %>% 
  st_union(wbronx[[1]][2,"geometry"]) %>%
  st_union()

vulnerability_catchment <- 
  st_intersect_summarize(
    CTs,
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(vulnerability_catchment,
                        st_erase(vulnerability_catchment, subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918) ,
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


##Find summary demographics for leading potential expansion areas based on subway access
accessibility_catchment <- bushwick[[1]][2, "geometry"] %>%
  st_union(cbronx[[1]][2,"geometry"]) %>% 
  st_union(chb[[1]][2,"geometry"]) %>%
  st_union(jhf[[1]][2,"geometry"]) %>% 
  st_union(jamaica[[1]][2,"geometry"]) %>% 
  st_union()

accessibility_catchment <- 
  st_intersect_summarize(
    CTs,
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(accessibility_catchment,
                        st_erase(accessibility_catchment, subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918) ,
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


##Find summary demographics for all target neighbourhoods
bike_total_catchment <- bushwick[[1]][2, "geometry"] %>% 
  st_union(cbronx[[1]][2,"geometry"]) %>% 
  st_union(chb[[1]][2,"geometry"]) %>%
  st_union(ebronx[[1]][2,"geometry"]) %>% 
  st_union(enyc[[1]][2,"geometry"]) %>% 
  st_union(rockaway[[1]][2,"geometry"]) %>% 
  st_union(jhf[[1]][2,"geometry"]) %>% 
  st_union(jamaica[[1]][2,"geometry"]) %>% 
  st_union(sbronx[[1]][2,"geometry"]) %>% 
  st_union(spbr[[1]][2,"geometry"]) %>% 
  st_union(umanhattan[[1]][2,"geometry"]) %>% 
  st_union(wbronx[[1]][2,"geometry"]) %>%
  st_union()

bike_total_catchment <- 
  st_intersect_summarize(
    CTs,
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(bike_total_catchment,
                        st_erase(bike_total_catchment, subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918) ,
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


neighbourhoods_network_demographics <- tibble(
      nbhd = c("bushwick", "cbronx", "chb", "ebronx", 
              "enyc", "rockaway", "jhf", "jamaica", 
              "sbronx", "spbr", "umanhattan", "wbronx"), 
      pop_no_subway_access = c((bushwick[[1]][1,2]/bushwick[[1]][2,2]), (cbronx[[1]][1,2]/cbronx[[1]][2,2]),  
                               (chb[[1]][1,2]/chb[[1]][2,2]), (ebronx[[1]][1,2]/ebronx[[1]][2,2]),  
                               (enyc[[1]][1,2]/enyc[[1]][2,2]), (rockaway[[1]][1,2]/rockaway[[1]][2,2]), 
                               (jhf[[1]][1,2]/jhf[[1]][2,2]), (jamaica[[1]][1,2]/jamaica[[1]][2,2]),
                               (sbronx[[1]][1,2]/sbronx[[1]][2,2]), (spbr[[1]][1,2]/spbr[[1]][2,2]),
                               (umanhattan[[1]][1,2]/umanhattan[[1]][2,2]), (wbronx[[1]][1,2]/wbronx[[1]][2,2])),
      geom = c(bushwick[[4]], cbronx[[4]], chb[[4]], ebronx[[4]], 
                enyc[[4]], rockaway[[4]], jhf[[4]], jamaica[[4]], 
                sbronx[[4]], spbr[[4]], umanhattan[[4]], wbronx[[4]])) %>%
  st_as_sf() %>%
  st_set_crs(26918) %>%
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(nbhd),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))



# Table 1. Leading potential expansion areas based on vulnerability index

table_2.1 <- 
  target_neighbourhoods_demographics %>% 
  st_collection_extract("POLYGON") %>%
  group_by(nbhd) %>% 
  st_drop_geometry() %>%
  summarize(vulnerability_index = first(vulnerability_index),
            pop_total = first(pop_total),
            pop_no_subway = first(pop_no_subway) * first(pop_total))


# Table 2. Leading potential expansion areas based on subway access

table_2.2 <- 
  target_subway_access %>% 
  filter(subway_service == FALSE) %>%
  st_collection_extract("POLYGON") %>%
  group_by(nbhd) %>% 
  summarize(pop_total = first(pop_total),
            geometry = st_union(geometry)) %>%
  mutate(area = st_area(.) %>% set_units(mi^2),
         access_per_mi = pop_total / area) %>%
  st_drop_geometry()

