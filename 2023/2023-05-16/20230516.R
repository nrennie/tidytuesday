library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(maps)

# load fonts
font_add_google("Ubuntu", "ubuntu")
font_add_google("Knewave", "knewave")
showtext_auto()

# read in data
tornados <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv")

# prep data
plot_data <- tornados |>
  mutate(decade = 10 * round(yr / 10)) |>
  filter(st %in% c("UT", "AZ", "NM", "CO")) |>
  select(om, decade, st) |>
  distinct() |>
  group_by(decade, st) |>
  summarise(n = n()) |>
  mutate(st = factor(st, levels = c("CO", "NM", "AZ", "UT")))

# start recording
gg_record(
  dir = file.path("2023", "2023-05-16", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 7, # width of saved image
  height = 7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#3B3561"
highlight_col <- "#BF3646"
light_col <- "#E0E0E0"

# text
title <- "Tornados"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = light_col,
  font_family = "ubuntu"
)
st <- "Of Arizona, New Mexico, Colorado, and Utah, Colorado 
is affected most by tornados. Colorado has also seen an increasing
number of tornados in recent decades, with 465 observed in the 
2000s.<br>**Data**: NOAA National Weather Service Storm Prediction
Center."

# plot
p2 <- ggplot() +
  geom_col(
    data = plot_data,
    mapping = aes(x = st, y = -n, group = decade),
    position = position_dodge(width = 0.9),
    fill = highlight_col,
    colour = light_col,
    linewidth = 0.1
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(x = st, y = 30, group = decade, label = decade),
    position = position_dodge(width = 0.9),
    colour = bg_col,
    size = 7,
    family = "ubuntu"
  ) +
  coord_polar(theta = "x") +
  labs(
    x = "",
    y = ""
  ) +
  scale_y_continuous(breaks = c(0, -200, -400),
                     labels = c(0, 200, 400)) +
  theme_minimal(base_size = 30) +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
p2

# get map
usa <- map_data("state")
corners <- usa |>
  filter(region %in% c("arizona", "colorado", "utah", "new mexico"))

p1 <- ggplot() +
  geom_polygon(
    data = corners,
    mapping = aes(
      x = long,
      y = lat,
      fill = region,
      group = group
    ),
    linewidth = 1,
    fill = light_col,
    colour = bg_col
  ) +
  labs(title = "Tornados",
       subtitle = st,
       caption = social) +
  theme_minimal(
    base_size = 36,
    base_family = "ubuntu"
  ) +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.title = element_text(
      family = "knewave",
      margin = margin(b = 20),
      colour = light_col,
      size = 90,
      hjust = 0.5
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.55,
      colour = light_col,
      family = "ubuntu",
      margin = margin(b = 0),
      hjust = 0.5,
      halign = 0.5
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      colour = light_col,
      margin = margin(t = 0),
      hjust = 0.5,
      halign = 0.5
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 10, 10, 25)
  )

# overlay
p1 + patchwork::inset_element(p2, 0, 0.00, 0.864, 0.97)

# save gif
gg_playback(
  name = file.path("2023", "2023-05-16", "20230516.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
