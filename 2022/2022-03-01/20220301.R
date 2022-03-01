library(tidyverse)
library(usefunc)
library(showtext)
library(geofacet)
library(cowplot)
library(sf)

# get data
tuesdata <- tidytuesdayR::tt_load('2022-03-01')
stations <- tuesdata$stations

# load fonts
font_add_google(name = "Bodoni Moda", family = "Bodoni MT")
showtext_auto()

#### Geofacet of number of stations per fuel time ####

# Prep data
plot_data1 <- stations %>% 
  select(STATE, FUEL_TYPE_CODE) %>% 
  filter(STATE %notin% c("ON", "PR")) %>% 
  group_by(STATE, FUEL_TYPE_CODE) %>% 
  summarise(n = n()) %>% 
  rename(state = STATE)

# Plot 
p1 <- ggplot(plot_data1, aes(x=n, y=FUEL_TYPE_CODE, fill=FUEL_TYPE_CODE)) +
  geom_col(width = 1) +
  facet_geo(~ state, grid = "us_state_grid2", label = "code") +
  scale_x_log10(breaks = c(1, 10, 1000)) +
  scale_y_discrete(limits=rev) +
  scale_fill_manual("", 
                    values = c("BD" = "#d57f70", "CNG" = "#ac9897",  "E85" = "#827498", "ELEC" = "#86af49", 
                               "HY" = "#af8c90", "LNG" = "#dcb967", "LPG" = "#b88bac"), 
                    labels = c("Biodiesel", "Compressed Natural Gas", "Ethanol", "Electric",
                               "Hydrogen", "Liquefied Natural Gas", "Propane")) +
  guides(fill=guide_legend(ncol=2)) +
  labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.background = element_rect(fill = "gray97", colour="gray97"),
        legend.background = element_rect(fill = "gray97", colour="gray97"),
        legend.key = element_rect(fill = "gray97", colour="gray97"),
        strip.background =element_rect(fill="gray97"),
        strip.text = element_text(colour = 'gray30', family="Bodoni MT", size=26),
        legend.text = element_text(colour = "gray50", size=24, hjust = 0, family="Bodoni MT"),
        legend.title = element_text(colour = "gray50", size=26, hjust = 0.5, family="Bodoni MT"),
        legend.position=c(1.45,0.11),
        plot.margin = unit(c(0.3, 20, 0.3, 0.7), "cm"), #top, right, bottom, left
        axis.text = element_text(colour = "gray60", size=24, hjust = 0.5, family="Bodoni MT"),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p1


#### Map of most common alternative fuel in each state ####
plot_data2 <- stations %>% 
  select(STATE, FUEL_TYPE_CODE) %>% 
  group_by(STATE, FUEL_TYPE_CODE) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(STATE) %>% 
  filter(STATE %notin% c("DC", "ON", "PR")) %>% 
  slice_max(n) 

# Join map data (from https://www.weather.gov/gis/USStates)
s_file <- st_read(dsn = "2022/2022-03-01/s_11au16/s_11au16.shp") 
map_data1 <- inner_join(s_file, plot_data2, by = "STATE")

# Plot data
p2 <- ggplot() +
  geom_sf(data=map_data1, 
          mapping = aes(fill = FUEL_TYPE_CODE), 
          colour="gray97", size = 0.3) +
  coord_sf(xlim = c(-180, -60)) +
  #scale_fill_manual("", values = c("E85" = "#827498", "ELEC" = "#86af49")) +
  theme(panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.background = element_rect(fill = "gray97", colour="gray97"),
        legend.position="none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        axis.title= element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p2


#### Join plots ####

add_plot <- "2022/2022-03-01/p_inline.jpeg"
q <- ggdraw() + 
  draw_plot(p1) +
  draw_image(add_plot, x = 0.8, y = 0.6, 
             hjust = 0.5, vjust = 1, halign = 0.5, valign = 1, width=0.25) +
  draw_label(x=0.8, y=0.9, hjust=0.5, "ALTERNATIVE FUEL STATIONS", 
             color = "gray30", fontface="bold", size = 50, fontfamily="Bodoni MT") +
  draw_label(x=0.8, y=0.75, hjust=0.5, str_wrap_break("Seven different alternative fuels are available in the United States, though not all are available in all states. The most common alternative fuel available at fuel stations in the United States is electric.\n\nN. Rennie | Data: US DOT", 50),
             color = "gray60", size = 42, lineheight = 0.3, fontfamily="Bodoni MT") 
q

ggsave(q, filename="2022/2022-03-01/20220301.jpg", height=12, width=20, unit="in")
