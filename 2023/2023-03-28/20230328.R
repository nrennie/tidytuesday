library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# load data
transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
timezones <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')
timezone_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezone_countries.csv')
countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/countries.csv')

# data wrangling
world <- ne_countries(scale = "medium", returnclass = "sf") 

timezones_sf <- timezones |> 
  select(-comments) |> 
  sf::st_as_sf(coords = c("longitude", "latitude")) |> 
  sf::st_set_crs(4326) |> 
  separate(zone, into = c("continent", "place"), sep="/")

# start recording
gg_record(
  dir = file.path("2023", "2023-03-28", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 5.1, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# text
title <- "<span style='font-size:44pt;font-family:Fraunces;color:#2F4F4F;'>Time Zones of the World</span><br>"
cap <- "Time zones tend to follow the boundaries between countries and their subdivisions
instead of strictly following longitude. For every one-hour time, a point on the earth moves
through 15 degrees of longitude. Each point relates to one of 337 time zones listed in the 
IANA time zone database. The colours show which time zones are in
<span style='color:#CC6677;'>Africa</span>, 
<span style='color:#332288;'>America</span>, 
<span style='color:#DDCC77;'>Antarctica</span>, 
<span style='color:#117733;'>Asia</span>, 
<span style='color:#88CCEE;'>Atlantic</span>, 
<span style='color:#882255;'>Australia</span>, 
<span style='color:#44AA99;'>Europe</span>, 
<span style='color:#999933;'>Indian</span>, and 
<span style='color:#AA4499;'>Pacific</span> zones.<br>Data: IANA tz database<br>"
social <- nrBrand::social_caption(bg_colour = nr_light)
caption <- paste0(title, cap, social)


# plot
ggplot() +
  geom_sf(data = world,
          colour = nr_dark,
          fill = alpha(nr_mid, 0.3)) +
  geom_sf(data = timezones_sf, size = 0.4,
          mapping = aes(colour = continent)) +
  geom_sf(data = timezones_sf, size = 1.6,
          pch = 21,
          fill = "transparent",
          mapping = aes(colour = continent)) +
  geom_segment(data = data.frame(x = seq(-180, 180, by = 15)),
               mapping = aes(x = x, y = -160, xend = x, yend = 100),
               linewidth = 0.2,
               colour = alpha(nr_mid, 0.2)) +
  scale_colour_manual(
    values = c("#CC6677", "#332288", "#DDCC77",
               "#117733", "#88CCEE", "#882255",
               "#44AA99", "#999933", "#AA4499")
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 15),
                     limits = c(-190, 190)) +
  coord_sf(expand = FALSE) +
  labs(tag = caption) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = nr_light, colour = nr_light),
    panel.background = element_rect(fill = nr_light, colour = nr_light),
    panel.grid = element_blank(),
    plot.tag.position = c(-0.01, 0.12),
    plot.tag = element_textbox_simple(family = "Commissioner",
                                    colour = nr_dark,
                                    hjust = 0,
                                    lineheight = 0.6,
                                    margin = margin(l = 15,
                                                    t = 5,
                                                    b = 10))
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-03-28", "20230328.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)








