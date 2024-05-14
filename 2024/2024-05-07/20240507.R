# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggforce)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-05-07")
rolling_stone <- tuesdata$rolling_stone


# Load fonts --------------------------------------------------------------

font_add_google("Goldman", "goldman")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey90"
text_col <- "black"
col_palette <- PrettyCols::prettycols("Neon", n = 6)
highlight_col <- col_palette[3]

body_font <- "goldman"
title_font <- "goldman"


# Data wrangling ----------------------------------------------------------

plot_data <- rolling_stone |>
  select(clean_name, album, rank_2020, release_year) |>
  arrange(rank_2020) |>
  slice_head(n = 25) |>
  mutate(decade = factor(floor(release_year / 10) * 10)) |>
  mutate(
    label = glue("#{rank_2020}<br>**{album}**<br>{clean_name}"),
    label = factor(label, levels = label),
    x = rep(seq(1, 6, length.out = 5), times = 5),
    y = rep(seq(1, 7, length.out = 5), each = 5)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-05-07", "recording"),
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
  font_colour = text_col,
  font_family = body_font
)
title <- "Greatest Albums of All Time"
st <- "**The 500 Greatest Albums of All Time** is a recurring opinion survey and
music ranking of the finest albums in history, compiled by the American magazine
Rolling Stone. The first list was published in a special issue of the magazine in
2003, with an updated edition published in 2020. Here, the top 25 albums from the
2020 edition are visualised, and coloured based on the album's decade of release."
cap <- paste0(
  "**Data**: Data is Plural<br>**Graphic**:", social
)


# Define legend -----------------------------------------------------------

legend_df <- data.frame(
  x = seq(2, 5, length.out = 6),
  y = rep(0, times = 6),
  decade = sort(unique(plot_data$decade)),
  label = paste0(sort(unique(plot_data$decade)), "s")
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.45),
    fill = "black"
  ) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.2, fill = decade)) +
  geom_circle(aes(x0 = x, y0 = y, r = 0.02),
    fill = "white", colour = NA
  ) +
  geom_textbox(
    aes(x = x, y = y + 0.42, label = label),
    lineheight = 0.45,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    vjust = 1,
    family = body_font,
    size = 6,
    maxwidth = 0.22
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  scale_y_reverse(limits = c(7.8, -0.5)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  # add legend
  geom_circle(
    data = legend_df,
    aes(x0 = x, y0 = y, r = 0.25),
    fill = "black"
  ) +
  geom_circle(
    data = legend_df,
    aes(x0 = x, y0 = y, r = 0.1, fill = decade),
    colour = "black"
  ) +
  geom_textbox(
    data = legend_df,
    aes(x = x, y = y - 0.5, label = label),
    lineheight = 0.45,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    vjust = 1,
    family = body_font,
    size = 6,
    maxwidth = 0.20
  ) +
  coord_fixed() +
  theme_void(base_family = body_font, base_size = 24) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2),
      face = "bold",
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = -10, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-05-07", paste0("20240507", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
