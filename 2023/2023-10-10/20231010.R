# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(osmdata)
library(sf)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-10")
haunted_places <- tuesdata$haunted_places


# Load fonts --------------------------------------------------------------

font_add_google("Nosifer", "nosifer")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"
highlight_col <- "#880808"

body_font <- "ubuntu"
title_font <- "nosifer"

# Data wrangling ----------------------------------------------------------

# most haunted city
haunted_places |>
  group_by(city) |>
  summarise(n = n()) |>
  arrange(desc(n))

# los angeles
plot_data <- haunted_places |>
  filter(city == "Los Angeles", state == "California") |>
  filter(location != "J.F.K. Library Third") |> # manually remove as incorrect
  drop_na() |>
  select(-c(starts_with("city_"), city, country, state, state_abbrev))

# get street map
bb <- getbb("Los Angeles, United States")
streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway", "primary",
      "secondary", "tertiary"
    )
  ) |>
  osmdata_sf()

small_streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "residential", "living_street",
      "unclassified",
      "service", "footway"
    )
  ) |>
  osmdata_sf()
creepr::creepr()

# convert to sf object
plot_sf <- plot_data |>
  filter(longitude >= bb["x", "min"] & longitude <= bb["x", "max"]) |>
  filter(latitude >= bb["y", "min"] & latitude <= bb["y", "max"]) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-10", "recording"),
  device = "png",
  width = 6,
  height = 9,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = highlight_col,
  font_family = body_font
)
title <- "Los Angeles"
st <- "Los Angeles is the most haunted city in the United States, with 60
locations listed as haunted places in the Shadowlands Haunted Places Index.<br>
**Data**: The Shadowlands Haunted Places Index | OpenStreetMap"
cap <- glue(
  "*\"{plot_data$description[20]}\"*<br><br>{social}"
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = small_streets$osm_lines,
    color = "#630606",
    size = 0.1,
    alpha = 0.4
  ) +
  geom_sf(
    data = streets$osm_lines,
    color = highlight_col,
    size = 0.4,
    alpha = 0.7
  ) +
  with_outer_glow(
    geom_sf(
      data = plot_sf,
      colour = text_col,
      size = 0.5,
    ),
    colour = "white",
    sigma = 10,
    expand = 7
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 30, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      colour = highlight_col,
      hjust = 0.5,
      halign = 0.5,
      face = "bold",
      size = 100,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-10", paste0("20231010", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
