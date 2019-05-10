### MAP MAKING #################################################################

#demographic maps

{
  
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
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("Average inside service area: $90000\nAverage outside service area: $55000",
             size = 1,
             position = c(0.003,.93))
  

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
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05))+
  tm_credits("White population inside service area: 52%\nWhite population outside service area: 26%",
             size = 1,
             position = c(0.003,.93))

 
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
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.7)) +
  tm_compass(position = c(.9, .05)) + 
  tm_credits("Average inside service area: 48%\nAverage outside service area: 19%",
             size = 1,
             position = c(0.003,.93))


tm_shape(CTs) +
  tm_polygons ("immigrant_percent", 
               textNA = "No Data", 
               title = "Non-Citizen Population", 
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
            legend.format = list(fun = function(x) paste0(formatC(x, digits = 0, format = "f"), "%")),
            legend.position = c(0.003, 0.65)) +
  tm_compass(position = c(.9, .05)) +
  tm_credits("Non-citizen population inside service area: 13%\nNon-citizen population outside service area: 18%",
             size = 1,
             position = c(0.003,.93))

}

#bivariateservicemap

