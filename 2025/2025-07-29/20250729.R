# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(imager)
library(ggpattern)
library(elementalist)


# Functions ---------------------------------------------------------------

fade_image <- function(img_path) {
  # Load image
  img <- load.image(img_path)
  # Dimensions
  w <- dim(img)[1]
  h <- dim(img)[2]
  d <- dim(img)[3]
  cc <- dim(img)[4]
  # Create fade vector from left (1) to right (0)
  fade_vec <- seq(0, 1, length.out = w)
  # Create empty array for fade mask
  fade_array <- array(0, dim = c(w, h, d, cc))
  # Fill fade mask: each row gets the same fade_vec (along x)
  for (ch in 1:cc) {
    for (y in 1:h) {
      fade_array[, y, 1, ch] <- fade_vec
    }
  }

  # Convert to cimg
  fade_mask <- as.cimg(fade_array)

  # Convert hex color to RGB values scaled 0-1
  col_hex <- "#8482ff"
  col_rgb <- col2rgb(col_hex) / 255
  col_img <- imager::as.cimg(array(rep(col_rgb, each = w * h), dim = c(w, h, 1, 3)))

  # Blend color and image based on fade mask
  # Result = fade_mask * image + (1 - fade_mask) * color
  img_faded <- fade_mask * img + (1 - fade_mask) * col_img

  # Plot result
  file_parts <- tools::file_path_sans_ext(basename(img_path))
  file_dir <- dirname(img_path)
  out_path <- file.path(file_dir, paste0(file_parts, "_faded.png"))

  # Save as PNG
  save.image(img_faded, out_path)
}


# Prep images with fade ---------------------------------------------------

all_images <- list.files("2025/2025-07-29/images/", full.names = TRUE)
for (i in all_images) {
  fade_image(i)
}


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-07-29")
movies <- tuesdata$movies
shows <- tuesdata$shows


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "black"
text_col <- "white"
highlight_col <- "#8482ff"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- movies |>
  filter(report == "2025Jan-Jun") |>
  arrange(desc(views)) |>
  slice_max(views, n = 10) |>
  select(title, views) |>
  mutate(
    id = factor(row_number()),
    views = round(views / 1000000),
    title = str_remove(title, "//.*"),
    title = str_trim(title),
    title = str_to_upper(title),
    img_path = glue("2025/2025-07-29/images/{id}_faded.png")
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-07-29", "recording"),
  device = "png",
  width = 5 * (1200 / 675),
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

tag <- glue(
  "<img src='2025/2025-07-29/images/logo.png' width='50'><br><br><span style='font-size: 30pt;'>MOST</span><br><span style='font-size: 30pt;'>WATCHED</span><br><span style='font-size: 30pt;'>MOVIES</span><br><br><span style='color: {highlight_col}; font-size: 12pt;'>BY VIEWS</span><br><span style='color: {highlight_col}; font-size: 12pt;'>(JANUARY - JUNE 2025)</span>"
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_col_theme(
    mapping = aes(
      x = views, y = id
    ),
    fill = highlight_col,
    element = element_rect_round(radius = 0.25)
  ) +
  # add image
  geom_rect_pattern(
    mapping = aes(
      xmin = views - 20, xmax = views,
      ymin = 11 - as.numeric(id) - 0.43,
      ymax = 11 - as.numeric(id) + 0.43,
      pattern_filename = I(img_path)
    ),
    colour = NA,
    pattern = "image",
    pattern_type = "expand"
  ) +
  geom_text(
    mapping = aes(
      x = views + 2, y = id, label = paste0(views, "M")
    ),
    colour = text_col,
    family = body_font,
    hjust = 0,
    fontface = "bold",
    size.unit = "pt",
    size = 12
  ) +
  geom_text(
    mapping = aes(
      x = 2, y = id, label = title
    ),
    colour = bg_col,
    family = body_font,
    hjust = 0,
    fontface = "bold",
    size.unit = "pt",
    size = 11
  ) +
  geom_text(
    mapping = aes(
      x = -4, y = id, label = id
    ),
    colour = text_col,
    family = body_font,
    hjust = 0.5,
    fontface = "bold",
    size.unit = "pt",
    size = 9
  ) +
  scale_x_continuous(limits = c(-5, 180), expand = expansion(0, 7)) +
  scale_y_discrete(limits = rev, expand = expansion(0, 1.5)) +
  labs(tag = tag) +
  theme_void(base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = highlight_col, colour = highlight_col),
    panel.background = element_rect_round(
      fill = bg_col, colour = bg_col,
      radius = grid::unit(0.05, "snpc"),
    ),
    plot.tag.position = c(0.77, 0.35),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-07-29", paste0("20250729", ".png")),
  width = 5 * (1200 / 675),
  height = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-07-29", paste0("20250729", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
