# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggdist)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-12-09")
qatarcars <- tuesdata$qatarcars


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Data wrangling ----------------------------------------------------------

plot_data <- qatarcars |>
  select(horsepower)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-12-09", "recording"),
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
title <- "Nine ways to visualise a distribution"
st <- "Visualising a distribution helps to show the overall shape and spread of the data in a way that summary statistics can't, and there are multiple ways to do so. All of these plot types can be used to compare distributions for different groups, either by stacking or layering the plots for each group. Other plot types for showing distributions, such as violin plots or raincloud plots, can be obtained by combining two or more of these charts."
cap <- paste0(
  "**Data**: qatarcars<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

## Histogram ----
p1 <- ggplot(data = plot_data) +
  geom_histogram(
    mapping = aes(x = horsepower),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.5
  ) +
  facet_wrap(~"Histogram") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(NA, NA)) +
  theme_minimal(base_size = 7, base_family = body_font) 

## Density ----
p2 <- ggplot(data = plot_data) +
  geom_density(
    mapping = aes(x = horsepower),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.5
  ) +
  facet_wrap(~"Density plot") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) 

## Box plot ----
p3 <- ggplot(data = plot_data) +
  geom_boxplot(
    mapping = aes(x = horsepower, y = ""),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.5
  ) +
  facet_wrap(~"Box and whisker plot") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank()
  )

## Barcode ----
p4 <- ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(
      xmin = horsepower - 1,
      xmax = horsepower + 1,
      ymin = 0, ymax = 1
    ),
    fill = highlight_col,
    colour = "transparent",
    alpha = 0.7
  ) +
  facet_wrap(~"Barcode plot") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

## Dot plot ----
p5 <- ggplot(data = plot_data) +
  geom_dots(
    mapping = aes(x = horsepower),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.7,
    side = "topright"
  ) +
  facet_wrap(~"Dot plot") +
  labs(x = "Horsepower") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(NA, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) 

## Beeswarm ----
p6 <- ggplot(data = plot_data) +
  geom_dots(
    mapping = aes(x = horsepower, y = "1"),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.7,
    side = "both",
    layout = "swarm"
  ) +
  facet_wrap(~"Beeswarm plot") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank()
  )

## Gradient fill ----
p7 <- ggplot(data = plot_data) +
  stat_gradientinterval(
    mapping = aes(x = horsepower, y = "1"),
    fill = highlight_col,
    colour = "transparent"
  ) +
  facet_wrap(~"Gradient fill") +
  labs(x = "Horsepower") +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

## Colour ramp ----
p8 <- ggplot(data = plot_data) +
  stat_interval(
    mapping = aes(
      x = horsepower,
      color_ramp = after_stat(level)
    ),
    .width = c(.50, .80, .95),
    colour = highlight_col,
    interval_size_range = c(25, 30),
  ) +
  scale_color_ramp_discrete(na.translate = FALSE) +
  facet_wrap(~"Intervals") +
  labs(
    x = "Horsepower",
    color_ramp = "Level"
  ) +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

## Filled density ----
p9 <- ggplot(data = plot_data) +
  stat_halfeye(
    mapping = aes(
      x = horsepower,
      fill_ramp = after_stat(level)
    ),
    .width = c(.50, .80, .95),
    fill = highlight_col,
    colour = "transparent"
  ) +
  scale_fill_ramp_discrete(na.translate = FALSE) +
  facet_wrap(~"Density with intervals") +
  labs(
    x = "Horsepower",
    fill_ramp = "Level"
  ) +
  scale_x_continuous(limits = c(0, 2000)) +
  theme_minimal(base_size = 7, base_family = body_font) +
  theme(
    axis.text.y = element_blank()
  )


# Combine -----------------------------------------------------------------

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 +
  plot_annotation(
    title = title, subtitle = st,
    caption = cap
  ) +
  plot_layout(ncol = 3) &
  theme(
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_blank(),
    strip.text = element_text(
      hjust = 0,
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.7)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-12-09", paste0("20251209", ".png")),
  width = 7,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-12-09", paste0("20251209", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
