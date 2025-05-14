# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-05-13")
vesuvius <- tuesdata$vesuvius


# Load fonts --------------------------------------------------------------

font_add_google("Passion One", "passion")
font_add_google("Noto Sans Display", "noto")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#FDE4D8"
text_col <- "#3A1503"
highlight_col <- "#F35C16"

body_font <- "noto"
title_font <- "passion"


# Data wrangling ----------------------------------------------------------

earthquake_data <- vesuvius |>
  filter(type == "earthquake") |>
  select(time, duration_magnitude_md) |>
  mutate(week = isoweek(time)) |>
  mutate(year = year(time)) |>
  filter(year >= 2013, year <= 2024) |>
  drop_na() |>
  mutate(energy = 10^((1.5 * duration_magnitude_md) + 4.8))

week_data <- earthquake_data |>
  group_by(year, week) |>
  summarise(tot_energy = sum(energy)) |>
  ungroup() |>
  mutate(md = (log10(tot_energy) - 4.8) / 1.5)

year_data <- earthquake_data |>
  group_by(year) |>
  summarise(tot_energy = sum(energy)) |>
  ungroup() |>
  mutate(md = (log10(tot_energy) - 4.8) / 1.5)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-05-13", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- "Seismic Events at Mount Vesuvius"
st <- "Mount Vesuvius is a quiescent volcano near Naples, Italy, best known for its devastating eruption in 79 AD that buried Pompeii and Herculaneum. Though not currently active, it still registers seismic activity and remains closely monitored due to its potential for future eruptions and proximity to densely populated areas. In this chart, darker colours indicate higher magnitudes of seismic activity. No clear seasonal pattern is displayed."
cap <- paste0(
  "**Data**: Open Data Portal of Istituto Nazionale di Geofisica e Vulcanologia<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

g_bottom <- ggplot() +
  geom_raster(
    data = week_data,
    mapping = aes(x = "1", y = week, fill = md)
  ) +
  annotate(
    "rect",
    xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 53.5,
    colour = text_col,
    fill = "transparent",
    linewidth = 0.5
  ) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  scale_y_reverse(
    breaks = c(1, 14, 27, 40),
    labels = c("Jan", "Apr", "Jul", "Oct")
  ) +
  facet_wrap(~year, nrow = 1, strip.position = "top") +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 11) +
  theme(
    axis.text.y = element_text(
      colour = text_col, margin = margin(r = 5),
      size = rel(0.8)
    ),
    legend.position = "none",
    strip.text = element_text(
      face = "bold", colour = text_col,
      margin = margin(b = 5)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )
g_bottom

g_top <- ggplot() +
  geom_tile(
    data = year_data,
    mapping = aes(x = "1", y = "1", fill = md),
    colour = text_col,
    linewidth = 0.5
  ) +
  scale_y_discrete(
    breaks = c("1"),
    labels = c("Jan")
  ) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  facet_wrap(~year, nrow = 1, strip.position = "top") +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 11) +
  theme(
    axis.text.y = element_text(
      colour = bg_col, margin = margin(r = 5),
      size = rel(0.8)
    ),
    strip.text = element_blank(),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )
g_top


# Combine -----------------------------------------------------------------

g_top + g_bottom +
  plot_layout(ncol = 1, heights = c(1, 5)) +
  plot_annotation(
    title = title,
    subtitle = st,
    caption = cap
  ) &
  theme(
    text = element_text(size = 9),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(5, 5, 5, 5)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-05-13", paste0("20250513", ".png")),
  height = 5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-05-13", paste0("20250513", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
