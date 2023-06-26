library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(maps)
library(ggspatial)

# load fonts
font_add_google("Charm", "Charm")
showtext_auto()

# read data
us_place_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv")
us_place_history <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv")

# map data
usa <- map_data("usa")

# data wrangline
plot_data <- us_place_names |>
  filter(stringr::str_detect(feature_name, "Paradise")) |>
  select(feature_name, prim_lat_dec, prim_long_dec) |>
  filter(
    prim_long_dec >= -130,
    prim_lat_dec > 20
  )

# start recording
gg_record(
  dir = file.path("2023", "2023-06-27", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#d5c2a6"
light_col <- "#edd8b9"
dark_col <- "#4c3c23"

# text
social <- nrBrand::social_caption(
  bg_colour = light_col,
  icon_colour = dark_col,
  font_colour = dark_col,
  font_family = "Charm"
)

# plot
g <- ggplot() +
  geom_polygon(
    data = usa,
    mapping = aes(x = long, y = lat, group = group),
    fill = light_col,
    alpha = 0.7,
    colour = dark_col
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(x = prim_long_dec, y = prim_lat_dec),
    colour = alpha(dark_col, 0.8),
    pch = 13
  ) +
  geom_textbox(
    data = data.frame(x = -79,
                      y = 48.5,
                      label = ""
    ),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0.5,
    halign = 0.5,
    vjust = 0.5,
    valign = 0.5,
    text.colour = dark_col,
    box.colour = dark_col,
    box.size = 1,
    colour = dark_col,
    lineheight = 0.7,
    box.r = unit(0, "pt"),
    fill = light_col,
    alpha = 0.7,
    minheight = 0.24,
    minwidth = 0.415,
  ) +
  geom_textbox(
    data = data.frame(x = -79,
                      y = 48.5,
                      label = paste0(
                        "<span style='font-size:38pt'>*Map of PARADISE*</span><br><span style='font-size:22pt'>*in the UNITED STATES of AMERICA*</span><br>",
                        social
                      )
    ),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0.5,
    halign = 0.5,
    vjust = 0.5,
    valign = 0.5,
    family = "Charm",
    text.colour = dark_col,
    box.colour = dark_col,
    size = 8,
    lineheight = 0.7,
    colour = dark_col,
    box.r = unit(0, "pt"),
    fill = light_col,
    alpha = 0.7,
    minheight = 0.2,
    minwidth = 0.4,
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_x_continuous(breaks = seq(-125, -70, by = 10),
                     limits = c(-130, -65)) +
  scale_y_continuous(breaks = seq(25, 50, by = 5),
                     limits = c(22.5, 52.5)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 40, base_family = "Charm") +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = dark_col,
      linewidth = 0.7
    ),
    panel.grid = element_line(
      colour = alpha(dark_col, 0.2),
      linewidth = 0.5
    ),
    axis.text.y = element_text(
      colour = alpha(dark_col, 0.5),
      margin = margin(l = 0, r = -25)
    ),
    axis.text.x = element_text(
      colour = alpha(dark_col, 0.5),
      margin = margin(b = 0, t = -22)
    ),
    plot.margin = margin(15, 15, -12.5, -5)
  )

# annotations
g +
  annotation_north_arrow(
    location = "br",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering(
      text_size = 14,
      text_family = "Charm",
      fill = c(bg_col, dark_col),
      line_col = dark_col,
      text_col = dark_col)
  ) +
  geom_textbox(
    data = data.frame(x = -97.5,
                      y = 37.5,
                      label = ""
    ),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0.5,
    halign = 0.5,
    vjust = 0.5,
    valign = 0.5,
    box.colour = dark_col,
    box.size = 1.5,
    box.r = unit(0, "pt"),
    fill = "transparent",
    minheight = 1.04,
    minwidth = 1.03,
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-06-27", "20230627.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
