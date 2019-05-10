### MAP MAKING #################################################################


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
      tm_fill(col = "#91C2F") +
  tm_shape(servicearea_2014)+
      tm_fill(col = "#B1DA94") +
  tm_shape(servicearea_2013)+
      tm_fill(col = "#D1F2BA") +
  tmap_mode(mode = "plot") +
  tm_layout(title = "Bikeshare Service Area Growth, 2013-2018", inner.margins = .1)
     
#station map with markers

#demographic maps

CTs$white_percent <- NA
CTs$white_percent <- (CTs$pop_white/ CTs$pop_total) * 100

CTs$education_percent <- NA
CTs$education_percent <- (CTs$education/ CTs$pop_total) * 100

CTs$immigrant_percent <- NA
CTs$immigrant_percent <- (CTs$immigrant/ CTs$pop_total) * 100



tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = " Median Household Income", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, 40000, 80000, 120000, 160000, 200000, 260000)) +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) + 
  tm_layout(inner.margins = .05, 
            frame = F,
            legend.outside = F,
            main.title = "Median Household Income and Citibike Service Area", 
            main.title.size = 1.5,
            legend.title.size = 1.2,
            legend.text.size = .8,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f"))),
            legend.position = c(0, 0.7)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("Average inside service area: $90000\nAverage outside service area: $55000",
             size = 1,
             position = c(0,.94))

?formatC


tm_shape(CTs) +
  tm_polygons("med_income", 
              text = "No Data", 
              title = "Median Household Income", 
              border.alpha = 0,
              palette = "Oranges",
              breaks = c(0, 30000, 60000, 90000, 120000, 150000, 180000, 260000)) +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) + 
  tm_layout(main.title = "Income and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0("$", formatC(x, digits = 0, format = "f")))) +
  tm_compass(position = c(.9, .05))

tm_shape(CTs) +
  tm_polygons ("white_percent", 
               textNA = "No Data", 
               title = "White Population", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) + 
  tm_layout(main.title = "Race and Citibike Service Area",
            inner.margins = 0.05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%"))) +
  tm_compass(position = c(.9, .05))


tm_shape(CTs) +
  tm_polygons ("education_percent", 
               textNA = "No Data", 
               title = "Population with Bachelor Degree or more", 
               border.alpha = 0,
               palette = "Oranges") +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) + 
  tm_layout(main.title = "Education and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%"))) +
  tm_compass(position = c(.9, .05))


tm_shape(CTs) +
  tm_polygons ("immigrant_percent", 
               textNA = "No Data", 
               title = "Immigrants", 
               border.alpha = 0,
               palette = "-Oranges") +
  tm_shape(servicearea_2018) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(main.title = "Non-Citizen Population and Citibike Service Area", 
            inner.margins = .05, 
            frame = F,
            legend.outside = F,
            legend.text.size = .9,
            legend.title.size = 1.3,
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%"))) +
  tm_compass(position = c(.9, .05))



tmaptools::palette_explorer()




