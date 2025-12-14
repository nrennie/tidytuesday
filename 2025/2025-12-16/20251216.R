# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(osmdata)
library(sf)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-12-16")
roundabouts_clean <- tuesdata$roundabouts_clean


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"
highlight_col <- "grey50"


# Data wrangling ----------------------------------------------------------


bb <- getbb("East Kilbride")
streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway", "primary"
    )
  ) |>
  osmdata_sf()

small_streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "secondary", "tertiary", "residential"
    )
  ) |>
  osmdata_sf()

roundabouts <- bb |>
  opq() |>
  add_osm_feature(
    key = "junction",
    value = "roundabout"
  ) |>
  osmdata_sf()

plot_data <- roundabouts_clean |>
  filter(country == "United Kingdom", town_city == "East Kilbride") |>
  select(name, lat, long) |>
  st_as_sf(coords = c("lat", "long"), crs = 4326, remove = FALSE)



# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-12-16", "recording"),
  device = "png",
  width = 5,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- glue("<span style='font-family:{title_font}; font-size: 22pt'>Roundabouts of the Polo Mint City</span>")
st <- glue("East Kilbride is nicknamed the *Polo Mint City* due to its numerous roundabouts that resemble the mint sweet. The <span style='color:#FFCF00;'>**Kittelson & Associates roundabouts database**</span> lists 11 roundabouts in East Kilbride. However, according to *Traffic Scotland*, as of 2022, there are 600 of them in the East Kilbride area. <span style='color:white;'>**OpenStreetMap**</span> lists a total of 1,679 roundabouts though many of them appear to be duplicates.")
cap <- paste0(
  title, "<br>", st, "<br><br>", "**Data**: {roundabouts} | OpenStreetMap | Traffic Scotland<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = small_streets$osm_lines,
    color = highlight_col,
    size = 0.1,
    alpha = 0.8
  ) +
  geom_sf(
    data = streets$osm_lines,
    color = highlight_col,
    size = 0.5
  ) +
  geom_sf(
    data = roundabouts$osm_points,
    color = text_col,
    size = 0.5
  ) +
  with_outer_glow(
    geom_sf(
      data = plot_data,
      fill = "#FFCF00",
      colour = "white",
      pch = 21,
      size = 2
    ),
    colour = "#FFCF00",
    sigma = 12,
    expand = 12
  ) +
  labs(caption = cap) +
  coord_sf(expand = FALSE, xlim = c(-4.24, -4.13),
           ylim = c(55.735, 55.78), 
           clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -25, t = 50),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-12-16", paste0("20251216", ".png")),
  width = 5,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-12-16", paste0("20251216", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
