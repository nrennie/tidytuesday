library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gghighlight)

# read in data
founder_crops <- readr::read_csv("2023/2023-04-18/data/founder_crops.csv")

# map
world <- ne_countries(scale = "medium", returnclass = "sf") |> 
  filter(subregion != "Antarctica")

# data wrangling
plot_data <- founder_crops |> 
  filter(source == "ORIGINS") |> 
  group_by(site_name) |> 
  slice_max(prop, n = 1) |> 
  select(category, site_name, latitude, longitude, prop) |> 
  ungroup()

# start recording
gg_record(
  dir = file.path("2023", "2023-04-18", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 13, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- nr_light
dark_col <- nr_dark
main_col <- nr_contrast

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = main_col,
  font_colour = dark_col
)
title <- "Neolithic Founder Crops"
cap <- "Eight *founder crops* — emmer wheat, einkorn wheat, barley, lentil, 
pea, chickpea, bitter vetch, and flax — have long been thought to have been 
the bedrock of Neolithic economies. The world map below shows site locations considered
in the Origins of Agriculture database, with sites highlighted based on
their highest proportion of crops from different categories shown in the magnified
versions on the right."
cap2 <- "Data: The Neolithic Founder Crops in
Southwest Asia: Research Compendium"
st <- paste(cap2, "<br><br>", social)

# plot
world_map <- ggplot() +
  geom_sf(data = world,
          colour = nr_dark,
          fill = alpha(nr_mid, 0.3)) +
  geom_rect(data = data.frame(),
            mapping = aes(xmin = 30.0, xmax = 48.6,
                          ymin = 29.8, ymax = 38.5),
            fill = alpha(nr_contrast, 0.3),
            colour = nr_dark) +
  labs(title = title,
       tag = cap,
       caption = st) +
  theme_minimal(base_size = 30) +
  theme(
    plot.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    panel.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    plot.title = element_textbox_simple(
      lineheight = 0.4,
      colour = dark_col,
      family = "Fraunces",
      hjust = 0,
      face = "bold",
      size = 60,
      margin = margin(b = 5, t = 5)
    ),
    plot.tag = element_textbox_simple(
      lineheight = 0.45,
      colour = dark_col,
      family = "Commissioner",
      hjust = 0,
      size = 28,
      margin = margin(b = 10)
    ),
    plot.tag.position = c(0.01, 0.8),
    axis.text = element_blank()
  )
#world_map

insets <- ggplot() +
  geom_sf(data = world,
          colour = nr_dark,
          fill = alpha(nr_mid, 0.2)) +
  geom_point(data = plot_data,
             mapping = aes(x = longitude,
                           y = latitude,
                           colour = category),
             size = 1) +
  facet_wrap(~category, nrow = 2, ncol = 2) +
  gghighlight(use_direct_label = F,
              unhighlighted_params = list(colour = alpha(nr_mid, 0.7))) +
  scale_colour_manual(values = rep(main_col, 4)) +
  coord_sf(xlim = c(30.0, 48.6),
           ylim = c(29.8, 38.5),
           expand = FALSE) +
  labs(x = "",
       y = "") +
  theme_minimal(base_size = 30) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    panel.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    strip.text = element_textbox_simple(
      lineheight = 0.4,
      colour = dark_col,
      family = "Commissioner",
      hjust = 0,
      face = "bold",
      size = 28
    ),
    axis.text.x = element_text(margin = margin(t = 0)),
    axis.text.y = element_text(margin = margin(r = 0)))
#insets

# join plots
world_map + insets +
  plot_layout(width = c(1, 1.3)) &
  theme(
    plot.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    panel.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.45,
      colour = dark_col,
      family = "Commissioner",
      hjust = 0,
      size = 28,
      valign = 3,
      vjust = 3,
      margin = margin(b = 10, t = -60, l = 3)
    ),
    plot.margin = margin(r = 10, b = -20, l = 10, t = 10)
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-04-18", "20230418.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
