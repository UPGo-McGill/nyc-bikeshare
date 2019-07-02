### BUILD CASE STUDIES #########################################################

### Rebuild previous objects using network geometries

## Create total subway service network for all of NYC

subway_total_catchment <- 
  map(networks, `$`, "subway_polygon") %>%
  do.call(c, .) %>%
  st_union()

bike_total_catchment <- 
  map(networks, `$`, "bike_polygon") %>%
  do.call(c, .) %>%
  st_union()

total_network_demographics <- 
  st_intersect_summarize(
    CTs,
    tibble(
      service = c("bike_total", "bike_only"),
      geometry = c(bike_total_catchment, 
                   st_erase(bike_total_catchment, subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918),
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


## Prepare target neighbourhood "top five" tables for report

nbhd_network_demographics <- 
  tibble(
    nbhd = target_neighbourhoods$nbhd,
    service = "total",
    geometry = map(networks, `$`, "bike_polygon") %>% do.call(c, .)
  ) %>%
  st_as_sf() %>%
  st_set_crs(26918) %>%
  rbind(
    tibble(
      nbhd = target_neighbourhoods$nbhd,
      service = "bike_only",
      geometry = st_erase(
        (map(networks, `$`, "bike_polygon") %>% do.call(c, .)),
        map(networks, `$`, "subway_polygon") %>% do.call(c, .) %>% st_union()
        )) %>%
      st_as_sf() %>%
      st_set_crs(26918)
  ) %>% 
  st_intersect_summarize(
    CTs,
    .,
    group_vars = vars(nbhd, service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index)) %>% 
  ungroup()

nbhd_network_demographics <- 
  nbhd_network_demographics %>% 
  mutate(area = st_area(geometry),
         area = set_units(area, mi^2) %>% drop_units) %>% 
  st_drop_geometry() %>%
  gather(variable, value, -(nbhd:service)) %>% 
  unite(temp, service, variable) %>%
  spread(temp, value) %>% 
  mutate(perc_no_subway = bike_only_pop_total / total_pop_total,
         bike_only_pop_density = bike_only_pop_total/total_area,
         total_pop_density = total_pop_total/total_area) %>%
  select(-bike_only_area, -total_area)



#Table 0. for executive summary
table_2_0 <- 
  nbhd_network_demographics %>% 
  mutate (bike_only_non_white = 1- bike_only_pop_white) %>%
  select(nbhd, total_pop_total, total_pop_density, bike_only_pop_total, 
         perc_no_subway, bike_only_non_white, bike_only_med_income) %>%
  mutate_at(vars(total_pop_total, total_pop_density, bike_only_pop_total, 
                 bike_only_med_income), 
            round, -2) %>% 
  mutate_at(vars( perc_no_subway, bike_only_non_white), 
            round, 3)  %>%
  set_names("Neighborhood", "Population", "Population per sq mi",
            "Total population without subway access",
            "Percentage of the population without subway access",
           "Non-white without subway access",
           "Median income without subway access")


# Table 1. Leading potential expansion areas based on vulnerability index

table_2_1 <- 
  nbhd_network_demographics %>% 
  select(nbhd, total_vulnerability_index, total_pop_total, bike_only_pop_total,
         bike_only_pop_density) %>%
  mutate_at(vars(total_pop_total, bike_only_pop_total, bike_only_pop_density), 
            round, -2) %>% 
  arrange(-total_vulnerability_index) %>% 
  set_names("Neighborhood", "Vulnerability index", "Total population",
            "Population without subway access",
            "Population without subway access per square mile")


# Table 2. Leading potential expansion areas based on subway access

table_2_2 <-
  nbhd_network_demographics %>% 
  select(nbhd, perc_no_subway, total_pop_total, bike_only_pop_total, 
         bike_only_pop_density) %>% 
  mutate_at(vars(total_pop_total, bike_only_pop_total, bike_only_pop_density), 
            round, -2) %>% 
  mutate(perc_no_subway = round(perc_no_subway, 3)) %>%
  arrange(-bike_only_pop_density) %>% 
  set_names("Neighborhood", "Percentage of population without subway access", 
            "Total population", "Population without subway access",
            "Population without subway access per square mile")



## Find summary demographics for expansion areas based on vulnerability index

vulnerability_catchment <- 
  st_intersect_summarize(
    CTs,
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(map(
             networks[table_2_1[1:5,]$Neighborhood], `$`, "bike_polygon") %>% 
               do.call(c, .) %>% st_union(),
             st_erase(map(networks[table_2_1[1:5,]$Neighborhood], `$`,
                          "bike_polygon") %>% 
                        do.call(c, .) %>% st_union(),
                      subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918),
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))


## Find summary demographics for expansion areas based on subway access

accessibility_catchment <- 
  st_intersect_summarize(
    CTs,
    tibble(service = c("bike_total", "bike_only"),
           geometry = c(map(
             networks[table_2_2[1:5,]$Neighborhood], `$`, "bike_polygon") %>% 
               do.call(c, .) %>% st_union(),
             st_erase(map(networks[table_2_2[1:5,]$Neighborhood], `$`,
                          "bike_polygon") %>% 
                        do.call(c, .) %>% st_union(),
                      subway_total_catchment))) %>%
      st_as_sf() %>%
      st_set_crs(26918),
    group_vars = vars(service),
    population = pop_total,
    sum_vars = vars(pop_white, education, poverty),
    mean_vars = vars(med_income, vulnerability_index))

  