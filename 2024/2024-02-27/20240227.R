# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-02-27")
events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "gray5"
highlight_col <- "#35978f"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

plot_births <- births |>
  mutate(type = "birth") |>
  rename(year = year_birth) |>
  filter(year >= 1900) |>
  mutate(label = glue("{person} ({str_trim(description)})")) %>%
  mutate(
    theta = seq(pi / 4, (7 / 4) * pi, length.out = nrow(.)),
    x = 6 * cos(theta),
    y = 6 * sin(theta),
    angle = 180 + 360 * (theta / (2 * pi))
  )

plot_deaths <- deaths |>
  mutate(type = "death") |>
  rename(year = year_death) |>
  filter(year >= 1900) |>
  mutate(label = glue("{person} ({str_trim(description)})")) %>%
  mutate(
    theta = seq(pi / 4, (7 / 4) * pi, length.out = nrow(.)),
    x = 3 * cos(theta),
    y = 3 * sin(theta),
    angle = 180 + 360 * (theta / (2 * pi))
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-02-27", "recording"),
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
title <- "Take a leap! Births and deaths on February 29<sup>th</sup>"
st <- glue("February 29 is a leap day (or 'leap year day'), an intercalary date
added periodically to create leap years in the Julian and Gregorian calendars. 
Wikpedia lists {nrow(plot_births)}
<span style='color: {highlight_col};'>births</span> and {nrow(plot_deaths)}
<span style='color: #bf812d;'>deaths</span> on a leap day since 1900.")
cap <- paste0(
  "**Data**: Wikipedia<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_text(
    data = plot_births,
    mapping = aes(
      x = x, y = y, angle = angle, label = person
    ),
    hjust = 1,
    family = body_font,
    size = 6,
    colour = highlight_col
  ) +
  geom_text(
    data = plot_deaths,
    mapping = aes(
      x = x, y = y, angle = angle, label = person
    ),
    hjust = 1,
    family = body_font,
    size = 6,
    colour = "#bf812d"
  ) +
  geom_textbox(
    data = data.frame(x = 0, y = 1.2, label = title),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0,
    colour = text_col,
    family = title_font,
    fontface = "bold",
    lineheight = 0.5,
    fill = "transparent",
    box.colour = "transparent",
    size = 11,
    minwidth = 0.5
  ) +
  geom_textbox(
    data = data.frame(x = 0, y = -0.5, label = st),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0,
    colour = text_col,
    family = body_font,
    lineheight = 0.5,
    fill = "transparent",
    box.colour = "transparent",
    size = 9,
    minwidth = 0.45
  ) +
  scale_x_continuous(limits = c(-8, 8)) +
  scale_y_continuous(limits = c(-8, 8)) +
  labs(caption = cap) +
  theme_void(base_size = 30, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = -5, l = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-02-27", paste0("20240227", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
