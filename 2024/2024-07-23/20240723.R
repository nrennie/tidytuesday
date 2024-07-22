# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtextcircle)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-07-23")
auditions <- tuesdata$auditions
eliminations <- tuesdata$eliminations
finalists <- tuesdata$finalists
ratings <- tuesdata$ratings
seasons <- tuesdata$seasons
songs <- tuesdata$songs


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu")
font_add_google("Passion One")
showtext_auto()

body_font <- "Ubuntu"
title_font <- "Passion One"


# Define colours ----------------------------------------------------------

bg_col <- "#00186d"
text_col <- "#fafafa"
highlight_col <- "#06c7ff"


# Data wrangling ----------------------------------------------------------

plot_data <- eliminations |>
  mutate(
    place = case_when(
      str_detect(place, "–") ~ str_extract(place, "(?<=–).*"),
      str_detect(place, "-") ~ str_extract(place, "(?<=-).*"),
      TRUE ~ place
    ),
    place = as.numeric(place)
  ) |> 
  filter(place <= 10, season <= 10) |> 
  select(contestant, place) |>
  mutate(
    place = if_else(place == "1", highlight_col, text_col)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-07-23", "recording"),
  device = "png",
  width = 7,
  height = 7,
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
st <- glue::glue(
  "All American Idol <span style='color: {highlight_col};'>winners</span>
  and contestants who made the top 10 in each of the first 10 seasons."
)
cap <- paste0(
  "**Data**: Wikipedia | **Image**: Wikipedia <br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_textcircle(
    data = plot_data,
    mapping = aes(label = contestant, colour = place),
    family = body_font,
    size = 8,
    r = 4
  ) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 0,
      y = 0.5,
      image = "2024/2024-07-23/logo.png"
    ),
    size = 0.3
  ) +
  geom_textbox(
    data = data.frame(x = 0, y = -1, label = st),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0.5,
    halign = 0.5,
    colour = text_col,
    family = body_font,
    lineheight = 0.5,
    fill = "transparent",
    box.colour = "transparent",
    size = 9,
    minwidth = 0.45
  ) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(caption = cap) +
  coord_fixed() +
  theme_void(base_size = 20) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = -5, l = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save image --------------------------------------------------------------

ggsave(
  "2024/2024-07-23/20240723.png",
  width = 7,
  height = 7
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-07-23", paste0("20240723", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
