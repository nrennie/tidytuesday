library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)
library(sf)

# read in data
production <- readr::read_csv("2023/2023-04-11/data/egg-production.csv")
cage_free <- readr::read_csv("2023/2023-04-11/data/cage-free-percentages.csv")

# data wrangling
# average monthly egg production per quarter (millions)
# cage free organic eggs only
plot_data <- production |>
  mutate(q_year = quarter(observed_month, type = "year.quarter"),
         q = quarter(observed_month)) |>
  filter(prod_process == "cage-free (organic)") |>
  select(q_year, q, n_eggs) |> 
  group_by(q_year) |>
  mutate(n = round(mean(n_eggs) / 1000000)) |> 
  select(-n_eggs) |> 
  unique()

# start recording
gg_record(
  dir = file.path("2023", "2023-04-11", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 4, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# egg
theta <- seq(0, 2 * pi, length.out = 1000)
a <- 1
b <- 3
k <- 70
r <- k * (cos(theta)^2 + a * cos(theta) + b)
egg_data <- tibble(
  x = r * sin(theta),
  y = -1 * r * cos(theta)
)
plot_egg <- egg_data |>
  mutate(
    x = 0.0115 * x + 2018.9,
    y = y + 350
  )

# make a polygon
egg_poly <- st_polygon(list(cbind(plot_egg$x, plot_egg$y / 70)))
egg_line <- st_linestring(matrix(c(plot_data$q_year, plot_data$n / 70), ncol = 2))
cropped_sf <- lwgeom::st_split(egg_poly, egg_line) %>%
  st_collection_extract(c("POLYGON"))

# colours
bg_col <- "#FFEDDE"
egg_col <- "peachpuff2"
line_col <- "sienna4"
dark_line_col <- "#1B0E07"

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = dark_line_col,
  font_colour = line_col
)
title <- "US Egg Production"
st <- "The cracked line in the egg below shows the average monthly production (in millions) 
of cagefree organic eggs in the USA. The data used in this infographic is based on reports produced by the 
United States Department of Agriculture, which are published weekly or monthly.<br><br>Data: The Humane League Labs US Egg Production Dataset"

ggplot() +
  geom_sf(
    data = cropped_sf[[1]],
    fill = egg_col,
    colour = line_col
  ) +
  geom_point(
    data = filter(plot_data, q == 1),
    mapping = aes(x = q_year,
                  y = n / 70),
    colour = line_col
  ) +
  geom_text(
    data = filter(plot_data, q == 1),
    mapping = aes(x = q_year,
                  y = n / 70,
                  label = paste0("Jan ", round(q_year), "\n", n)),
    colour = dark_line_col,
    size = 8,
    lineheight = 0.8,
    fontface = "bold",
    family = "Commissioner",
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = social,
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    panel.background = element_rect(
      colour = bg_col, fill = bg_col
    ),
    plot.title = element_textbox_simple(
      lineheight = 0.4,
      colour = dark_line_col,
      family = "Fraunces",
      hjust = 0,
      size = 50,
      margin = margin(b = 5, t = -20)
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.45,
      colour = line_col,
      family = "Commissioner",
      hjust = 0,
      size = 24,
      margin = margin(b = 5)
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.4,
      colour = line_col,
      family = "Commissioner",
      hjust = 0,
      size = 24,
      margin = margin(t = 10, b = -30)
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-04-11", "20230411.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
