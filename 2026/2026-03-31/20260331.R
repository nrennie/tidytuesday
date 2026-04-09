# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggimage)
library(MetBrewer)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-03-31")
ocean_temperature <- tuesdata$ocean_temperature
ocean_temperature_deployments <- tuesdata$ocean_temperature_deployments


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


# Data wrangling ----------------------------------------------------------

temp_data <- ocean_temperature |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  # filter(year == 2025) |>
  select(month, sensor_depth_at_low_tide_m, mean_temperature_degree_c) |>
  group_by(month, sensor_depth_at_low_tide_m) |>
  summarise(med = median(mean_temperature_degree_c)) |>
  ungroup()

depths <- sort(unique(temp_data$sensor_depth_at_low_tide_m))
previous_depth <- function(x) {
  ind <- which(depths == x)
  if (ind == 1) {
    return(0)
  } else {
    return(depths[ind - 1])
  }
}
midpoints <- function(x) {
  return((x[-length(x)] + x[-1]) / 2)
}

plot_data <- temp_data |>
  rowwise() |>
  mutate(start = previous_depth(sensor_depth_at_low_tide_m)) |>
  ungroup()


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Ocean surface temperatures peak earlier in the year compared to deeper water"
st <- "Median daily coastal ocean temperatures recorded at 7 depths. Nova Scotia, Canada. 2018 - 2025."
cap <- paste0("**Image**: Philip Graves @ Unsplash<br>", source_caption(source = "Coastal Monitoring Program. Centre for Marine Applied Research. Nova Scotia Open Data Portal.", graphic = social))


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  # add ocean photo
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = midpoints(c(0.5, 12.5)),
      y = midpoints(c(0, 40)),
      image = "2026/2026-03-31/ocean_bw.jpg"
    ),
    size = 1.1
  ) +
  geom_rect(mapping = aes(
    xmin = month - 0.5, xmax = month + 0.5,
    ymin = start,
    ymax = sensor_depth_at_low_tide_m, fill = med
  ),
  alpha = 0.5) +
  geom_hline(
    yintercept = depths,
    colour = bg_col,
    linewidth = 0.75
  ) +
  geom_vline(
    xintercept = 1.5:11.5,
    colour = bg_col,
    linewidth = 0.75
  ) +
  scale_fill_met_c("Hiroshige", direction = -1) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_y_reverse(breaks = rev(midpoints(c(0, depths))),
                  labels = rev(depths)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL,
    y = "Sensor depth at low tide (m)"
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 11, base_family = body_font) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    plot.margin = margin(5, 15, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.3)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 30, t = 10),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 15),
      family = body_font
    ),
    axis.title.y = element_text(angle = 0, vjust = 1.05,
                                margin = margin(r = -145)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-03-31", paste0("20260331", ".png"))
)
