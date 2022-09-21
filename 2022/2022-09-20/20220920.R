library(tidyverse)
library(showtext)
library(camcorder)
library(sf)
library(rnaturalearth)
library(PrettyCols)

# load fonts
font_add_google("Ubuntu", "ubuntu")
font_add_google("Special Elite", "elite")
showtext_auto()

# load data
HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

# prep data
plot_data <- HydroWASTE_v10 %>% 
  filter(COUNTRY == "United Kingdom") %>% 
  select(LAT_WWTP, LON_WWTP, WASTE_DIS) 

# start recording
gg_record(
  dir = file.path("2022", "2022-09-20", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 8, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# background map
world_sf <- ne_countries(returnclass = "sf", scale = "large")
uk_sf <- ne_states(country = "united kingdom", returnclass = "sf")

# transform to spatial data
plot_sf <- st_as_sf(plot_data,
                    coords = c("LON_WWTP", "LAT_WWTP"),
                    crs = 4326,
                    remove = FALSE)

# tag
tg <- usefunc::str_wrap_break("There are 1,887 wastewater treatment plants listed as being located in the United Kingdom.\n\nOnly one discharges more than 500,000 cubic metres of waste, with the mean volume being just over 8,000 cubic metres.\n\nN. Rennie\n\nData: Macedo et al, 2022\n\n#TidyTuesday", 35)

# plot
ggplot() +
  geom_sf(data = uk_sf,
          colour = "black",
          fill = "#fafafa",
          size = 0.2) +
  geom_sf(data = plot_sf, 
          mapping = aes(colour = log(1+WASTE_DIS)), 
          size = 0.5) +
  labs(x = "", 
       y = "", 
       title = "Wastewater Plants in the United Kingdom", 
       tag = tg) +
  guides(colour = guide_colourbar(title.position = "top", 
                                  title = bquote('Treated wastewater discharged '(m^3)),
                                  direction = "horizontal")) +
  scale_colour_pretty_c("Greens",
                        direction = -1, 
                        limits = c(1, log(800001)),
                        breaks = c(1, log(10001), log(800001)),
                        labels = c(0, "10K", "800K")
                        ) +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank(),
        panel.grid = element_blank(), 
        legend.position = c(-0.12,-0.1),
        legend.key.width = unit(0.8, "cm"),
        plot.tag.position = c(-0.41, 0.5),
        plot.title = element_text(hjust = 0.96,
                                  vjust = 20,
                                  size = 56, 
                                  family = "elite"), 
        plot.tag = element_text(hjust = 0,
                                vjust = 0.5,
                                size = 30, 
                                lineheight = 0.5,
                                family = "ubuntu"), 
        legend.text = element_text(hjust = 0.5,
                                   size = 36, 
                                   family = "ubuntu"), 
        legend.title = element_text(hjust = 0.5,
                                   size = 30, 
                                   family = "ubuntu"), 
        plot.margin = unit(c(0.5, 1.5, 0.5, 4.5), unit = "cm"), 
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"), 
        legend.background = element_rect(fill = "#fafafa", colour = "#fafafa"))

# save gif
gg_playback(
  name = file.path("2022", "2022-09-20","20220920.gif"),
  frame_duration = .25
)
