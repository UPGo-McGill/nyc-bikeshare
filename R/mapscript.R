install.packages("tmap")
library(tmap)

#get basemap files

{ nyc_counties <- get_acs(
  geography = "county", 
  variables = c(pop_white = "B02001_002"),
  year = 2017, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  geometry = TRUE)

nyc_counties <-
  nyc_counties %>%
  st_transform(26918) %>% 
  st_erase(ny_water) 


nyc_city <- 
  st_union(nyc_counties)

}


#serviceareaboundaries through time

{
servicearea_2013 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2013) %>%
                     st_buffer(300) %>%
                     st_union() %>% 
                     st_erase(ny_water))


servicearea_2014 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2014) %>%
                     st_buffer(300) %>%
                     st_union()%>% 
                     st_erase(ny_water))

servicearea_2015 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2015) %>%
                     st_buffer(300) %>%
                     st_union()%>% 
                     st_erase(ny_water))

servicearea_2016 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2016) %>%
                     st_buffer(300) %>%
                     st_union()%>% 
                     st_erase(ny_water))

servicearea_2017 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2017) %>%
                     st_buffer(300) %>%
                     st_union()%>% 
                     st_erase(ny_water))

servicearea_2018 <- 
  suppressWarnings(station_list %>%
                     filter(Year == 2018) %>%
                     st_buffer(300) %>%
                     st_union()%>% 
                     st_erase(ny_water))

}


#service area growth map

tm_shape(nyc_city) +
tm_fill(col = "#E8E8E8") +
  tm_shape(nyc_counties)+
      tm_borders(col = "white") +
  tm_shape(servicearea_2018) +
      tm_fill(col = "#327A00") +
  tm_shape(servicearea_2017)  +
      tm_fill(col = "#519225") +
  tm_shape(servicearea_2016)+
      tm_fill(col = "#71AA4A") +
  tm_shape(servicearea_2015)+
      tm_fill(col = "#91C26F") +
  tm_shape(servicearea_2014)+
      tm_fill(col = "#B1DA94") +
  tm_shape(servicearea_2013)+
      tm_fill(col = "#D1F2BA") +
  tmap_mode(mode = "view") +
  tm_add_legend()


#median income map



