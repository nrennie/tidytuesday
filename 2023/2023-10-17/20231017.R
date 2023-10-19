# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(statebins)
library(ggimage)
library(tayloRswift)
library(ggnewscale)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums


# Load fonts --------------------------------------------------------------

font_add_google("Caveat", "caveat")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey95"
highlight_col <- tayloRswift::swift_palettes$lover[1]


# Data wrangling ----------------------------------------------------------

album_order <- taylor_album_songs |> 
  select(album_name, album_release) |> 
  distinct() |> 
  arrange(album_release) |> 
  pull(album_name)

plot_data <- taylor_album_songs |>
  group_by(album_name) |>
  summarise(energy = round(100 * mean(energy, na.rm = TRUE) / 5)) |>
  mutate(album_name = factor(album_name, levels = album_order)) |>
  uncount(energy, .remove = FALSE) |>
  group_by(album_name) |>
  mutate(y = row_number()) |>
  ungroup()

bg_data <- expand.grid(
  album_name = album_order,
  y = 1:20
) |>
  mutate(album_name = factor(album_name, levels = album_order)) |>
  as_tibble()

image_data <- plot_data |>
  select(album_name) |>
  distinct() |>
  mutate(
    img = str_replace(album_name, " \\s*\\([^\\)]+\\)", ""),
    img = str_to_lower(str_replace(img, " ", "_")),
    img = paste0(img, ".png"),
    img = file.path("2023", "2023-10-17", "images", img)
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-10-17", "recording"),
  device = "png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

body_font <- "caveat"

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
st <- "<span style='font-size: 44pt;'>The average energy, as defined by Spotify, for each of Taylor Swift's albums.</span>"
cap <- paste0(
  st, "<br>**Data**: {taylor}<br>", social
)


# Plot --------------------------------------------------------------------

# Adapted from: https://stackoverflow.com/questions/64355877/round-corners-in-ggplots-geom-tile-possible
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile",
  statebins:::GeomRrect,
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  default_aes = aes(
    fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA, width = NA, height = NA
  ),
  required_aes = c("x", "y"),
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  draw_key = draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      ...
    )
  )
}

ggplot() +
  geom_rtile(
    data = bg_data,
    mapping = aes(x = album_name, y = y, fill = y),
    alpha = 0.2,
    width = 0.95,
    height = 0.85
  ) +
  scale_fill_gradientn(colours = tayloRswift::swift_palettes$lover[4:1]) +
  new_scale_fill() +
  geom_rtile(
    data = plot_data,
    mapping = aes(x = album_name, y = y, fill = y),
    width = 0.95,
    height = 0.85
  ) +
  geom_rtile(
    data = plot_data,
    mapping = aes(x = album_name, y = -y, fill = y),
    alpha = 0.4,
    width = 0.95,
    height = 0.85
  ) +
  scale_fill_gradientn(colours = tayloRswift::swift_palettes$lover[4:1],
                       limits = c(1, 14)) +
  geom_image(
    data = image_data,
    mapping = aes(x = album_name, y = 24, image = img),
    asp = 1,
    size = 0.11
  ) +
  labs(caption = cap) +
  theme_void(base_size = 38) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-10-17", paste0("20231017", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
