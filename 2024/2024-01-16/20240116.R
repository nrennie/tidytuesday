# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(sf)
library(maps)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-16")
polling_places <- tuesdata$polling_places


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Carter One", "carter")
showtext_auto()


# Define colours and fonts-------------------------------------------------

text_col <- "#202A44"
blue_col <- "#0015BC"
red_col <- "#C41E3A"
bg_col <- "#fafafa"
col_palette <- c(
  monochromeR::generate_palette(red_col, "go_lighter", n_colours = 2),
  "white",
  rev(monochromeR::generate_palette(blue_col, "go_lighter", n_colours = 2))
  )
col_df <- data.frame(n = c(1:5, NA), colour = c(col_palette, "#ababab"))

body_font <- "roboto"
title_font <- "carter"

# Data wrangling ----------------------------------------------------------

polling_counties <- polling_places |>
  filter(election_date == "2020-11-03") |>
  select(state, county_name) |>
  group_by(county_name) |>
  mutate(n = n()) |>
  ungroup() |>
  mutate(n = ntile(n, 5)) |>
  distinct() |>
  mutate(state = usefunc::US_abb_to_name(state)) |>
  mutate(county_name = str_to_sentence(county_name))

# get map
usa_map <- map("county", plot = FALSE, fill = TRUE)
usa_sf <- usa_map |>
  st_as_sf() |>
  separate_wider_delim(ID, names = c("state", "county_name"), delim = ",") |>
  mutate(
    county_name = str_to_sentence(county_name),
    state = str_to_sentence(state)
  )

# join data
plot_data <- usa_sf |>
  left_join(polling_counties, by = c("state", "county_name")) |>
  st_as_sf() |> 
  left_join(col_df, by = "n")


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-16", "recording"),
  device = "png",
  width = 7,
  height = 6,
  units = "in",
  dpi = 300
)


# Legend ------------------------------------------------------------------

leg_data <- data.frame(
  x = c(-123:-119, -110),
  y = rep(50.5, 6),
  colour = col_df$colour
)

leg_text <- data.frame(
  x = c(-126, -118, -109),
  y = c(50.5, 50.5, 50.5),
  label = c("Less", "More", "Unavailable")
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = red_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "US Polling Places"
st <- "For the November 3, 2020 election in the United States, data on 3,994 
polling places* was collected along with the county it is located in. The distribution 
of polling places per county represented in the data is highly skewed, with 
many counties having under 200. Cook county in Georgia, has the most polling places 
listed at 3,602.<br><br>
*Several states (Colorado, Hawaii, Oregon, Washington and Utah) vote primarily
by mail and have little or no data in this dataset, and others were not available
for other reasons."
cap <- paste0(
  "**Data**: The Center for Public Integrity<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = plot_data,
    mapping = aes(fill = colour),
    colour = text_col
  ) +
  statebins:::geom_rtile(
    data = leg_data,
    mapping = aes(x = x, y = y, fill = colour),
    colour = text_col,
    radius = grid::unit(3, "pt"),
    width = 0.9,
    height = 0.8
  ) +
  # Add legend labels
  geom_text(
    data = leg_text,
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 8,
    hjust = 0
  ) +
  scale_fill_identity() +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_family = body_font, base_size = 26) +
  theme(
  plot.margin = margin(5, 10, 5, 10),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  plot.title = element_textbox_simple(
    colour = text_col,
    hjust = 0,
    halign = 0,
    margin = margin(b = 5, t = 0),
    lineheight = 0.5,
    size = rel(1.8),
    family = title_font
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    hjust = 0,
    halign = 0,
    margin = margin(b = 10, t = 5),
    lineheight = 0.5,
    family = body_font
  ),
  plot.caption = element_textbox_simple(
    colour = text_col,
    hjust = 0,
    halign = 0,
    margin = margin(b = 0, t = 10),
    lineheight = 0.5,
    family = body_font
  )
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-16", paste0("20240116", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
