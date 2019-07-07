## Font fixing

fonts_to_fix <- read_csv(system.file("fontmap", "fonttable.csv", package="extrafontdb"))

fonts_to_fix <- 
  fonts_to_fix %>% 
  mutate(FamilyName = if_else(str_detect(FontName, "Condensed") == TRUE,
                              "Futura Condensed", FamilyName))

write_csv(fonts_to_fix, system.file("fontmap", "fonttable.csv", package="extrafontdb"))

extrafont::loadfonts()

str(pdfFonts(), max.level=1)




base_map_gg <- 
  ggplot() +
  geom_sf(data = nyc_msa, fill = "#F0F0F0", colour = "#F0F0F0", lwd = 0) +
  geom_sf(data = nyc_city, fill = "grey80", colour = "grey80", lwd = 0) +
  annotation_scale(location = "br", width_hint = 0.4, line_col = "grey50",
                   bar_cols = c("grey50", "white"), unit_category = "imperial",
                   style = "ticks") +
  theme(legend.position = c(0.01, .99),
        legend.justification = c(0, 1),
        legend.spacing = unit(0, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        text = element_text(family = "Futura"),
        title = element_text(family = "Futura Condensed", face = "bold", size = 15),
        strip.text = element_text(family = "Futura", face = "bold", size = 12),
        legend.title = element_text(family = "Futura Condensed", face = "bold", 
                                    size = 15),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) 


figure_2[[01]] <- 
  base_map_gg +
  geom_sf(data = target_neighbourhoods, mapping = aes(fill = nbhd), 
          colour = "white") +
  geom_sf(data = st_sf(bike = "Existing Citi Bike service area", 
                       geometry = bike_service_areas_no_holes$geometry[3]),
          mapping = aes(colour = bike), fill = "grey40") +
  geom_sf(data = subway_lines, colour = alpha("grey90", 0.75)) +
  scale_fill_manual(name = "Figure 1. Proposed bike sharing expansion areas",
                    values = nbhd_colours) +
  scale_colour_manual(name = NULL, values = "white") +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +
  gg_bbox(nyc_city, 0.01, .99, 0.04, 1.02)

ggsave("output/report-2/figure_2_01.pdf", plot = figure_2[[01]], height = 8,
       width = 8, units = "in", dpi = 300)

ggsave("output/report-2/figure_2_01.eps", plot = figure_2[[01]], height = 8,
       width = 8, units = "in", dpi = 300)

quartz()
