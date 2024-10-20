# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggragged)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-10-08")
most_visited_nps_species_data <- tuesdata$most_visited_nps_species_data


# Load fonts --------------------------------------------------------------

font_add_google("Freckle Face", "Freckle")
font_add_google("Commissioner")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey95"
bg_col2 <- "grey75"
highlight_col <- "#1F7A8C"
text_col <- "grey5"

body_font <- "Commissioner"
title_font <- "Freckle"


# Data wrangling ----------------------------------------------------------

# Facets: x = Slug/Snail Type, y = National Park
# Bar chart: x = Number of species, y = Abundance

ParkLevels <- most_visited_nps_species_data |>
  filter(CategoryName == "Slug/Snail") |>
  select(ParkName, Order) |>
  unique() |>
  count(ParkName) |>
  arrange(-n) |>
  pull(ParkName)

plot_data <- most_visited_nps_species_data |>
  filter(CategoryName == "Slug/Snail") |>
  select(ParkName, Order, Abundance) |>
  count(ParkName, Order, Abundance) |>
  mutate(
    Abundance = replace_na(Abundance, "Unknown"),
    Abundance = factor(Abundance,
      levels = c(
        "Abundant", "Common", "Uncommon", "Rare", "Unknown"
      ),
      labels = c(
        "Abundant", "Common", "Uncommon", "Rare", "Unknown / Not stated"
      )
    )
  ) |>
  mutate(
    ParkName = factor(
      ParkName,
      levels = ParkLevels,
      labels = str_wrap(ParkLevels, 14)
    )
  ) |>
  drop_na(Order)




# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-10-08", "recording"),
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
  font_family = title_font,
  twitter = NA
)
title <- "Where are all the slugs and snails?"
st <- "Only eight of the fifteen most visited US National Parks have recorded
observations of slugs or snails. The Great Smoky Mountains National Park has
the highest number of different orders of slugs or snails in the database,
with a high number of families in the Helicida order - including those that
are common, uncommon, and rare."
cap <- paste0(
  "**Data**: National Parks Service (NPS)<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = n, y = Abundance)
) +
  geom_col(fill = highlight_col) +
  facet_ragged_rows(
    vars(ParkName),
    vars(Order)
  ) +
  labs(
    x = NULL, y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 80),
    labels = c(0, 50, "No.\nfamilies")
  ) +
  scale_y_discrete(limits = rev) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 28, base_family = body_font) +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(
      fill = bg_col,
      colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col2,
      colour = bg_col2
    ),
    plot.title = element_text(
      family = title_font,
      size = rel(2)
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      colour = text_col,
      width = 0.95,
      hjust = 0,
      margin = margin(b = 5, t = -5),
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.5,
      hjust = 0,
      family = title_font,
      halign = 0,
      margin = margin(t = 5)
    ),
    strip.text.y = element_text(
      face = "bold",
      family = title_font,
      angle = 0,
      hjust = 0,
      margin = margin(l = 2),
      lineheight = 0.3,
      size = rel(1.3)
    ),
    strip.text.x = element_text(
      margin = margin(0.1, 0, 0.1, 0, "cm")
    ),
    axis.text.x = element_text(
      margin = margin(t = -5), hjust = 0,
      lineheight = 0.3
    ),
    axis.text.y = element_text(margin = margin(r = -2)),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.3, "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10, 5, 10, 10)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-10-08", paste0("20241008", ".png")),
  width = 7,
  height = 7,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2024", "2024-10-08", paste0("20241008", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
