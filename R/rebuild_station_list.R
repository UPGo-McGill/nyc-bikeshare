### Rebuilding the station_list file

## Missing many steps, and then:

predicates <- st_intersects(station_list, CTs)
station_list <- station_list[which(lengths(predicates)!=0),]