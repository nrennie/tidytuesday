# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(magick)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-03-24")
pi_digits <- tuesdata$pi_digits


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
st <- "*Pi* is an irrational number, meaning its decimal representation never ends and never settles into a permanent repeating pattern.<br><br>"


# Function ----------------------------------------------------------------

# n - number of rows/columns
# highlight - digits to highlight

pi_plot <- function(n = 10, highlight = 0:9, size = 18) {
  # Data
  plot_data <- pi_digits |>
    slice_head(n = n^2) |>
    mutate(
      row = rep(1:n, each = n),
      col = rep(1:n, times = n)
    )
  # Text
  title <- glue("<span style='font-family:{title_font};font-size:18pt;'>**The first {n^2} digits of π**</span><br><br>")
  cap <- paste0(title, st, source_caption(source = "PiDay", graphic = social, sep = " | "))
  # Plot
  g <- ggplot(
    data = plot_data,
    mapping = aes(
      x = col, y = row, label = digit,
      colour = digit %in% highlight
    )
  ) +
    geom_text(
      family = body_font, size.unit = "pt",
      size = size
    ) +
    scale_y_reverse(limits = c(n + 0.5, 0.5)) +
    scale_x_continuous(limits = c(0.5, n + 0.5)) +
    scale_colour_manual(
      values = c("TRUE" = text_col, "FALSE" = "#2A3850")
    ) +
    labs(caption = cap) +
    coord_fixed() +
    theme_void(base_size = 11, base_family = body_font) +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      legend.position = "none",
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 10),
        family = body_font
      ),
      strip.text = element_textbox_simple(
        face = "bold",
        margin = margin(t = 10),
        size = rel(0.9)
      ),
      panel.grid.minor = element_blank()
    )
  return(g)
}


# Plot --------------------------------------------------------------------

pi_plot(n = 10, highlight = 1, size = 20) +
  canvas(
    width = 5, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-03-24", paste0("20260324", ".png"))
)


# Gif ---------------------------------------------------------------------

h_vals <- c(list(0:9), as.list(0:9))
for (i in 1:length(h_vals)) {
  p <- pi_plot(highlight = h_vals[[i]])
  fname <- glue('2026/2026-03-24/gifs/{str_pad(i, 2, "left", "0")}.png')
  ggsave(fname, p, width = 5, height = 6)
}

frames <- lapply(sprintf("2026/2026-03-24/gifs/%02d.png", 1:11), image_read)
gif_frames <- list()
n_morph <- 5
n_hold <- 10 
for (i in 1:(length(frames) - 1)) {
  hold <- rep(list(frames[[i]]), n_hold)
  morphed <- image_morph(c(frames[[i]], frames[[i+1]]), frames = n_morph)
  gif_frames <- c(gif_frames, hold, as.list(morphed))
}
gif_frames <- c(gif_frames, rep(list(frames[[11]]), n_hold))
gif_animated <- image_animate(image_join(gif_frames), fps = 20, optimize = TRUE)
image_write(gif_animated, file.path("2026", "2026-03-24", paste0("20260324", ".gif")))

unlink("2026/2026-03-24/gifs/", recursive = TRUE)


# Bigger gif --------------------------------------------------------------

h_vals <- c(list(0:9), as.list(0:9))
for (i in 1:length(h_vals)) {
  p <- pi_plot(n = 50, highlight = h_vals[[i]], size = 7)
  fname <- glue('2026/2026-03-24/gifs/{str_pad(i, 2, "left", "0")}.png')
  ggsave(fname, p, width = 5, height = 6)
}

frames <- lapply(sprintf("2026/2026-03-24/gifs/%02d.png", 1:11), image_read)
gif_frames <- list()
n_morph <- 5
n_hold <- 10 
for (i in 1:(length(frames) - 1)) {
  hold <- rep(list(frames[[i]]), n_hold)
  morphed <- image_morph(c(frames[[i]], frames[[i+1]]), frames = n_morph)
  gif_frames <- c(gif_frames, hold, as.list(morphed))
}
gif_frames <- c(gif_frames, rep(list(frames[[11]]), n_hold))
gif_animated <- image_animate(image_join(gif_frames), fps = 20, optimize = TRUE)
image_write(gif_animated, file.path("2026", "2026-03-24", paste0("20260324-50", ".gif")))

unlink("2026/2026-03-24/gifs/", recursive = TRUE)


