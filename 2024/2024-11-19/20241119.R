# Load packages -----------------------------------------------------------

library(tidyverse)
library(camcorder)
library(ggtext)
library(glue)
library(emoji)
library(cowplot)
library(systemfonts)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-11-19")
episode_metrics <- tuesdata$episode_metrics


# Load fonts --------------------------------------------------------------

register_font(
  "Font Awesome 6 Brands",
  plain = "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

register_font(
  "Poppins",
  plain = "fonts/Poppins/Poppins-Regular.ttf",
  bold = "fonts/Poppins/Poppins-Bold.ttf",
  italic = "fonts/Poppins/Poppins-Italic.ttf"
)


# Define colours and fonts-------------------------------------------------

bg_col <- "#f8df20"
text_col <- "#070606"
highlight_col <- "#ee2631"

body_font <- "Poppins"


# Data wrangling ----------------------------------------------------------

# https://stackoverflow.com/questions/22312207/how-to-assign-cut-range-midpoints-in-r
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub(
    "\\(|\\)|\\[|\\]", "",
    as.character(cut_label)
  ), ","))))
}

# prep data
plot_data <- episode_metrics |>
  select(season, unique_words) |>
  mutate(season = factor(season, levels = 1:14)) |>
  group_by(season) |>
  mutate(bin = cut_width(unique_words, width = 15)) |>
  mutate(midpoint = sapply(bin, get_midpoint)) |>
  ungroup() |>
  select(season, unique_words, midpoint) |>
  arrange(season, midpoint) |>
  group_by(season, midpoint) |>
  mutate(y = row_number()) |>
  ungroup()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-11-19", "recording"),
  device = "png",
  width = 4,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#ee2631;'>&#xf4f6;</span><span style='color:#f8df20;'>.</span><span style='font-family:Poppins;color:#070606;'>fosstodon.org/@nrennie</span><span style='color:#f8df20;'>...........</span><span style='font-family:\"Font Awesome 6 Brands\";color:#ee2631;'>&#xf08c;</span><span style='color:#f8df20;'>.</span><span style='font-family:Poppins;color:#070606;'>nicola-rennie</span><span style='color:#f8df20;'>........</span><span style='font-family:\"Font Awesome 6 Brands\";color:#ee2631;'>&#xe671;</span><span style='color:#f8df20;'>..</span><span style='font-family:Poppins;color:#070606;'>nrennie</span><span style='color:#f8df20;'>........</span><span style='font-family:\"Font Awesome 6 Brands\";color:#ee2631;'>&#xf09b;</span><span style='color:#f8df20;'>.</span><span style='font-family:Poppins;color:#070606;'>nrennie</span><span style='color:#f8df20;'>..</span>"
st <- "Bob's Burgers is an American animated sitcom that follows the Belcher family as they run a struggling hamburger restaurant. Across the first ten seasons the number of unique words in each episodes grey slightly."
cap <- paste0(
  "**Data**: {bobsburgersR} <br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

g <- ggplot(
  data = plot_data,
) +
  geom_density(
    mapping = aes(x = unique_words, y = after_stat(scaled)),
    fill = highlight_col,
    colour = highlight_col,
    linewidth = 0.1
  ) +
  geom_text(
    size = 1.5,
    mapping = aes(x = midpoint, y = -y / max(y), label = emoji("hamburger"))
  ) +
  facet_wrap(~season, ncol = 1, strip.position = "left") +
  scale_x_continuous(
    limits = c(800, 1600),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-1.1, 1.1),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = st,
    caption = cap,
    x = "Number of unique words per episode",
    y = "Season\n"
  ) +
  theme_minimal(base_size = 8, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 15, t = 70),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 15),
      family = body_font
    ),
    axis.title.y = element_text(
      angle = 0,
      face = "bold",
      margin = margin(r = -10)
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.3),
      linewidth = 0.3
    ),
    panel.grid.minor.x = element_blank(),
    strip.text.y.left = element_text(
      angle = 0
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0, "lines")
  )
g


# Add image ---------------------------------------------------------------

# image from https://en.m.wikipedia.org/wiki/File:Bob%27s_Burgers_logo.png#/media/File%3ABob's_Burgers_logo.svg
logo <- "2024/2024-11-19/logo.png"

final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(g) +
  cowplot::draw_image(
    logo,
    x = 0.5, y = 0.9,
    hjust = 0.5, vjust = 0,
    halign = 0.5, valign = 0,
    width = 0.5
  )
final_plot


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2024", "2024-11-19", paste0("20241119", ".png")),
  height = 8,
  width = 4
)

gg_playback(
  name = file.path("2024", "2024-11-19", paste0("20241119", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
