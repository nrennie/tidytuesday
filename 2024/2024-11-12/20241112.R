# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-12")
countries <- tuesdata$countries
country_subdivisions <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "black"
text_col <- "white"
col_palette <- PrettyCols::prettycols("Disco")[1:5]
highlight_col <- col_palette[5]

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

cols <- grDevices::colorRampPalette(col_palette)(length(LETTERS))
names(cols) <- LETTERS

plot_data <- countries |>
  arrange(alpha_3) |> 
  select(alpha_3, name) |>
  separate_wider_position(alpha_3, c("a" = 1, "b" = 1, "c" = 1)) |>
  mutate(
    facet_grp = as.character(rep(1:3, each = 83)),
    y = rep(1:83, times = 3)
  )

letter_data <- plot_data |>
  pivot_longer(
    cols = c(a, b, c),
    names_to = "letter_pos",
    values_to = "value"
  ) |> 
  mutate(
    x = rep(1:3, times = 249)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-11-12", "recording"),
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  twitter = NA
)
title <- "ISO 3166-1 Country Codes"
st <- "ISO 3166 is a standard published by the International Organization for
Standardization (ISO) that defines codes for the names of countries, dependent
territories, special areas of geographical interest, and their principal
subdivisions (e.g., provinces or states). "
cap <- paste0(
  "**Data**: {ISOcodes}<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------


ggplot() +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = 3.6, y = y, label = name
    ),
    hjust = 0,
    size = 5,
    colour = text_col,
    family = body_font
  ) +
  geom_raster(
    data = letter_data,
    mapping = aes(
      x = x, y = y, fill = value
    )
  ) +
  geom_text(
    data = letter_data,
    mapping = aes(
      x = x, y = y, label = value
    ),
    hjust = 0.5,
    size = 6,
    family = title_font,
    colour = bg_col
  ) +
  facet_wrap(
    ~facet_grp,
    ncol = 3
  ) +
  scale_fill_manual(
    values = cols
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(0.5, 10.5)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 25) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      face = "bold",
      size = rel(1.7),
      margin = margin(b = 5, t = 0),
      lineheight = 0.5,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-11-12", paste0("20241112", ".png")),
  bg = bg_col,
  width = 6,
  height = 8
)

gg_playback(
  name = file.path("2024", "2024-11-12", paste0("20241112", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
