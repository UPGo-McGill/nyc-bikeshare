### Rebuilding the station_list file

# Importing an earlier version of the station list
station_list <-
  st_read("data/station_list.csv") %>%
  as_tibble() %>%
  st_as_sf() %>% 
  select(-WKT) %>% 
  st_set_crs(26918)

station_list <- 
  station_list %>% 
  slice(st_intersects(
    station_list %>%
      filter(ID == "NULL"), station_list)
    %>% unlist() * -1)

id_3240 <- station_list %>% 
  filter(ID == 3240) %>% 
  slice(2) %>%
  mutate(Year = "2016")

station_list <- 
  station_list %>% 
  filter(!(ID == 3240 & Year == 2016)) %>% 
  filter(ID != 3248) %>%
  rbind(id_3240) %>% 
  arrange(ID, Year)

predicates <- st_intersects(station_list, CTs)
station_list <- station_list[which(lengths(predicates)!=0),]


st_write(station_list, "station_list.csv", layer_options = "GEOMETRY=AS_WKT")
