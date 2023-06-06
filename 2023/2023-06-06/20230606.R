library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)
library(nrBrand)

# load fonts
font_add_google("Roboto", "Roboto")
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

# read in data
owid_energy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv")

# prep data
plot_data <- owid_energy |>
  filter(year %in% c(1960, 1980, 2000, 2020)) |>
  filter(!is.na(iso_code)) |> 
  select(country, year, coal_prod_per_capita) |>
  filter(coal_prod_per_capita != 0) |> 
  drop_na() |> 
  pivot_wider(values_from = coal_prod_per_capita,
              names_from = year) |> 
  drop_na() |> 
  mutate(across(`1960`:`2020`, ~ 100 * (.x - `1960`)/ `1960`)) |> 
  pivot_longer(cols = `1960`:`2020`,
               values_to = "coal_prod_per_capita",
               names_to = "year") |> 
  mutate(year = as.numeric(year))

# start recording
gg_record(
  dir = file.path("2023", "2023-06-06", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4.5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- "#ffffff"
dark_col <- "#555656"
highlight_col <- "#b20e10"
light_col <- "#bec0c2"

# text
title <- "Coal Production per Capita"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "Roboto"
)
st <- "Global demand for coal is at an all-time high, but looking at the
changes in coal production per capita show that the story of coal production
is very different across the globe. In the last 20 years, Indonesia has 
become a leading exporter of coal."
cap <- paste0("**Data**: Our World in Data <br>**Graphic**: ", social)
highlight_country <- "Indonesia"

# plot
ggplot(plot_data) +
  geom_line(
    mapping = aes(
      x = year, y = coal_prod_per_capita, group = country
    ),
    colour = light_col,
    linewidth = 0.4
  ) +
  geom_line(
    data = filter(plot_data, country == highlight_country),
    mapping = aes(
      x = year, y = coal_prod_per_capita, group = country
    ),
    colour = highlight_col,
    linewidth = 0.4
  ) +
  geom_segment(
    mapping = aes(x = year, xend = year,
                  y = -100, yend = 25000)
  ) +
  geom_text(
    data = data.frame(
      x = 2015,
      y = pull(filter(plot_data, country == highlight_country, year == 2020), coal_prod_per_capita),
      label = highlight_country),
    mapping = aes(x = x, y = y, label = label),
    colour = highlight_col,
    family = "Roboto Slab",
    size = 7
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "",
    y = "% change per capita since 1960"
  ) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     limits = c(1959.9, 2020.1)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(
    base_size = 30,
    base_family = "Roboto"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    plot.title = element_textbox_simple(
      family = "Roboto Slab",
      face = "bold",
      margin = margin(t = 10, b = 15),
      colour = dark_col,
      hjust = 0,
      halign = 0,
      lineheight = 0.4,
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      colour = dark_col,
      margin = margin(b = 20),
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      colour = dark_col,
      hjust = 0,
      halign = 0
    ),
    axis.text = element_text(family = "Roboto Slab"),
    axis.title = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(dark_col, 0.5),
      linewidth = 0.5,
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(5, 15, 5, 10),
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-06-06", "20230606.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
