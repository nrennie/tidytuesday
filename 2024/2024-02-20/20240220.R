# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(sf)
library(VoronoiPlus)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-02-20")
isc_grants <- tuesdata$isc_grants


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- "#1c68bc"

cols_vec <- rcartocolor::carto_pal(length(unique(grant_data$year)) + 1, "Prism")[1:length(unique(grant_data$year))]
names(cols_vec) <- unique(grant_data$year)

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

grant_data <- isc_grants |>
  select(year, funded, title)

vor_data <- voronoi_treemap(funded ~ year + title, data = grant_data)
set.seed(1234)
groups <- filter(vor_data, level == 1)
subgroups <- filter(vor_data, level == 2) |>
  group_by(group) |>
  mutate(alpha = runif(1, 0, 0.6)) |>
  ungroup()

year_labels <- groups |>
  select(group, x, y) |>
  st_as_sf(coords = c("x", "y")) |>
  group_by(group) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") |>
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) |>
  st_drop_geometry()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-02-20", "recording"),
  device = "png",
  width = 4.5,
  height = 5.8,
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
title <- glue("<br><span style='font-size: 48px; font-weight: bold; font-family:{title_font};'>R Consortium ISC Grants</span><br><br>")
st <- glue("<span style='font-family:{body_font};'>The R Consortium Infrastructure Steering Committee (ISC) Grant Program
has been awarding grants since 2016. They will accept proposals again between
March 1 and April 1, 2024 (and then again in the fall). Each polygon represents a different project, 
with the size of the area representing the funding amount.</span><br><br>")
cap <- paste0(
  title, st,
  "**Data**: R Consortium ISC<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = group, fill = group),
    colour = text_col,
    linewidth = 0.9
  ) +
  geom_polygon(
    data = subgroups,
    mapping = aes(x = x, y = y, group = group, alpha = alpha),
    fill = bg_col,
    colour = text_col,
    linewidth = 0.3
  ) +
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = group),
    colour = "transparent",
    fill = bg_col,
    alpha = 0.3,
    linewidth = 0.9
  ) +
  geom_text(
    data = year_labels,
    mapping = aes(x = x, y = y, label = group),
    colour = text_col,
    family = title_font,
    fontface = "bold",
    size = 12
  ) +
  scale_alpha_identity() +
  scale_fill_manual(values = cols_vec) +
  labs(
    caption = cap
  ) +
  coord_equal() +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(-16, 0, 0, 0),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = text_col, colour = text_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, t = 8),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-02-20", paste0("20240220", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
