
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-07-08")
answers <- tuesdata$answers
color_ranks <- tuesdata$color_ranks
users <- tuesdata$users


# Load fonts --------------------------------------------------------------

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest = file.path("fonts", "xkcd.ttf"), mode = "wb"
)
sysfonts::font_add(
  family = "xkcd",
  regular = "fonts/xkcd.ttf"
)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = "white",
  icon_colour = "black",
  font_colour = "black",
  font_family = "xkcd",
  mastodon = NA
)
title <- "xkcd Colors"
st <- "The 100 most common RGB monitor colors, as defined in the 2010 xkcd Color Name Survey."
cap <- paste0(
  "**Data**: xkcd Color Survey<br>**Graphic**:", social
)


# Data wrangling ----------------------------------------------------------

set.seed(1234)

n <- 100
rows <- 10
cols <- 10
jitter_width <- 0.05

plot_data <- color_ranks |> 
  arrange(rank) |> 
  slice_head(n = n) |> 
  mutate(color = factor(color, levels = color)) |>
  mutate(
    x = rep(1:cols, times = rows)[1:n],
    y = rep(seq(from = rows, by = -1.5, length.out = rows), each = cols)[1:n],
    x0 = x - 0.4 + runif(n, -jitter_width, jitter_width), # bottom left
    x1 = x + 0.4 + runif(n, -jitter_width, jitter_width), # bottom right
    x2 = x + 0.4 + runif(n, -jitter_width, jitter_width), # top right
    x3 = x - 0.4 + runif(n, -jitter_width, jitter_width), # top left
    x4 = x0,
    y0 = y - 0.4 + runif(n, -jitter_width, jitter_width), # bottom left
    y1 = y - 0.4 + runif(n, -jitter_width, jitter_width), # bottom right
    y2 = y + 0.4 + runif(n, -jitter_width, jitter_width), # top right
    y3 = y + 0.4 + runif(n, -jitter_width, jitter_width), # top left
    y4 = y0,
  ) |>
  pivot_longer(
    cols = x0:x4, names_to = "name_x", values_to = "poly_x"
  ) |> 
  pivot_longer(
    cols = y0:y4, names_to = "name_y", values_to = "poly_y"
  ) |> 
  mutate(name_x = parse_number(name_x),
         name_y = parse_number(name_y)) |> 
  filter(name_x == name_y) |> 
  select(-c(name_x, name_y))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = plot_data,
    mapping = aes(
      x = poly_x, y = poly_y,
      group = hex,
      fill = hex
    ),
    colour = "black"
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(x = x, y = y + 0.5, label = str_wrap(color, 8)),
    family = "xkcd",
    lineheight = 0.7,
    vjust = 0,
    size = 2.6
  ) +
  scale_fill_identity() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed(clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_line(colour = "black"),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    strip.background = element_blank(),
    text = element_text(size = 15, family = "xkcd", colour = "black"),
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(
      fill = "grey99", colour = "grey99"
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      face = "bold",
      margin = margin(b = 5, t = 5),
      lineheight = 1.1,
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 20, t = 5),
      lineheight = 1,
      size = rel(0.75)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 15),
      lineheight = 1,
      size = rel(0.6)
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-07-08", paste0("20250708", ".png")),
  height = 7.5,
  width = 5,
  bg = "white",
  units = "in",
  dpi = 300
)
