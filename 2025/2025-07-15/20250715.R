# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(PrettyCols)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-07-15")
bl_funding <- tuesdata$bl_funding


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#BCE0F0"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

plot_data <- bl_funding |>
  select(year, total_y2000_gbp_millions, gia_as_percent_of_peak_gia) |>
  mutate(
    year = year + 1,
    decade = year - year %% 10
  ) |>
  filter(decade >= 2000) |>
  # bookcase
  arrange(desc(year)) |>
  group_by(decade) |>
  mutate(
    ymin = cur_group_id(),
    ymax = ymin + ((total_y2000_gbp_millions - min(total_y2000_gbp_millions)) /
      (max(total_y2000_gbp_millions) - min(total_y2000_gbp_millions)) * 0.9),
    xmin = year %% 10,
    xmax = xmin + 1
  ) |>
  ungroup()

gia2000 <- plot_data |> 
  slice_min(year) |> 
  pull(gia_as_percent_of_peak_gia)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-07-15", "recording"),
  device = "png",
  width = 5,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = "#227396",
  icon_colour = "#F8F9F1",
  font_colour = "#F8F9F1",
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
cap <- paste0(
  "**Data**: www\\.gov\\.uk | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

offset <- 0.037
ggplot() +
  # floor
  annotate(
    "rect", xmin = -1, xmax = 11,
    ymin = 0.2, ymax = 0.7,
    fill = "#227396"
  ) +
  annotate(
    "rect", xmin = -1, xmax = 11,
    ymin = 0.55, ymax = 0.7,
    fill = "#F8F9F1"
  ) +
  # signpost
  annotate(
    "rect", xmin = 3, xmax = 7,
    ymin = 4.3, ymax = 4.7,
    fill = "grey70"
  ) +
  annotate(
    "text", x = 5, y = 4.57, label = "British Library",
    colour = "grey10", family = title_font,
    fontface = "bold",
    size = 5
  ) +
  annotate(
    "text", x = 5, y = 4.4, label = "Funding",
    colour = "grey10", family = body_font,
    size = 4.5
  ) +
  annotate("segment",
           x = c(3.2, 6.8), xend = c(3.2, 6.8),
           y = 4.9, yend = 4.65,
           colour = "grey10",
           linewidth = 1
  ) +
  # bookshelf
  annotate("rect",
    xmin = 0 - 5 * offset, xmax = 10 + 5 * offset,
    ymin = 1 - offset, ymax = 4 + offset,
    fill = "#624526",
    colour = "#49341D",
    linewidth = 4
  ) +
  annotate("segment",
    x = 0 - 5 * offset, xend = 10 + 5 * offset,
    y = c(2, 3) - offset, yend = c(2, 3) - offset,
    colour = "#49341D",
    linewidth = 4
  ) +
  annotate("segment",
           x = c(2, 8), xend = c(1.5, 8.5),
           y = c(0.95, 0.95), yend = c(0.4, 0.4),
           colour = "#49341D",
           linewidth = 4,
           lineend = "round"
  ) +
  # books
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax,
      fill = gia_as_percent_of_peak_gia
    ),
    colour = "#624526",
    linewidth = 0.1
  ) +
  # annotations
  geom_textbox(
    data = data.frame(),
    mapping = aes(
      x = 7.5, y = 3.5,
      label = "Each book represents a year, with the 2000s on the bottom shelf, and 2020s on the top shelf."
    ),
    fill = "transparent",
    box.colour = "transparent",
    colour = "#F8F9F1",
    hjust = 0.5,
    halign = 0.5
  ) +
  geom_textbox(
    data = data.frame(),
    mapping = aes(
      x = 7.5, y = 2.5,
      label = "The height of the books represents the total funding received by the British Library (adjusted to the year 2000)."
    ),
    fill = "transparent",
    box.colour = "transparent",
    colour = "#F8F9F1",
    hjust = 0.5,
    halign = 0.5
  ) +
  annotate("rect", xmin = 3.03, xmax = 4.25, ymin = 1.43, ymax = 1.54,
           fill = alpha("#F8F9F1", 0.8)) +
  annotate("rect", xmin = 4.7, xmax = 5.75, ymin = 1.43, ymax = 1.54,
           fill = alpha("#F8F9F1", 0.8)) +
  geom_textbox(
    data = data.frame(),
    mapping = aes(
      x = 3, y = 1.6,
      label = "The colour of the books represents whether the percentage of funding which is core funding from the UK government was <span style='color:#1b7837;'>**higher**</span> or <span style='color:#762a83; background: coral;'>**lower**</span> compared to 2000."
    ),
    fill = "transparent",
    box.colour = "transparent",
    colour = "#F8F9F1",
    hjust = 0.5,
    halign = 0.5,
    minwidth = 0.55
  ) +
  # styling
  scale_y_continuous(limits = c(0.2, 4.9)) +
  scale_fill_pretty_div(palette = "PurpleGreens", midpoint = gia2000) +
  coord_cartesian(expand = FALSE) +
  labs(caption = cap) +
  theme_void(base_family = body_font) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = "#F8F9F1",
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = -20, b = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-07-15", paste0("20250715", ".png")),
  height = 7,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-07-15", paste0("20250715", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
