# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-07-30")
summer_movie_genres <- tuesdata$summer_movie_genres
summer_movies <- tuesdata$summer_movies


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#750D37"
dark_col <- "#EB6534"
highlight_col <- "#E2C044"
na_col <- "#A0D2DB"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

all_genres <- summer_movie_genres |>
  drop_na(genres) |>
  count(genres) |>
  arrange(desc(n)) |>
  pull(genres)

genre_mat <- matrix(NA, nrow = length(all_genres), ncol = length(all_genres))
for (i in 1:length(all_genres)) {
  for (j in 1:length(all_genres)) {
    if (i == j) {
      genre_mat[i, j] <- sum((summer_movies$genres == all_genres[i]), na.rm = TRUE)
    } else if (i < j) {
      xi <- str_detect(summer_movies$genres, all_genres[i])
      xj <- str_detect(summer_movies$genres, all_genres[j])
      genre_mat[i, j] <- sum(xi * xj, na.rm = TRUE)
    }
  }
}
rownames(genre_mat) <- all_genres
colnames(genre_mat) <- all_genres

plot_data <- genre_mat |>
  as.data.frame() |>
  rownames_to_column(var = "y") |>
  as_tibble() |>
  pivot_longer(cols = -y, names_to = "x") |>
  mutate(
    x = factor(x, levels = all_genres),
    y = factor(y, levels = all_genres)
  ) |>
  drop_na()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-07-30", "recording"),
  device = "png",
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  twitter = NA
)
title <- "Ready for a summer drama?"
st <- "Movies in the Internet Movie Database with the word **summer** in their title
are most likely be a *drama*. Movies can be assigned to multiple genres, and this heatmap
shows the number of movies with different combinations of genres.* The number of movies
with a single genre are shown on the diagonal.** "
notes <- glue("<br>* Some movies are represented more than once as they have
more than two genres.<br>
** Movies with no listed genre are not included. <span style='color:{na_col}'>Blue</span> represents no movies in this genre combination.")
cap <- paste0(
  "**Data**: IMDb | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_raster(
    data = plot_data,
    mapping = aes(x = x, y = y, fill = value)
  ) +
  annotate(
    "text",
    x = 0.5, y = length(all_genres) + 0.5,
    label = title,
    family = title_font,
    colour = text_col,
    size = 18,
    hjust = -0.01,
    vjust = 1,
    fontface = "bold"
  ) +
  geom_textbox(
    data = data.frame(
      x = 0.5, y = length(all_genres) - 1.0,
      label = st
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 11,
    lineheight = 0.4,
    hjust = 0,
    halign = 0,
    vjust = 1,
    valign = 1,
    width = unit(3.8, "inch"),
    box.colour = "transparent",
    fill = "transparent"
  ) +
  geom_textbox(
    data = data.frame(
      x = 0.5, y = length(all_genres) - 6,
      label = notes
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 11,
    lineheight = 0.4,
    hjust = 0,
    halign = 0,
    vjust = 1,
    valign = 1,
    width = unit(3, "inch"),
    box.colour = "transparent",
    fill = "transparent"
  ) +
  geom_textbox(
    data = data.frame(
      x = 0.5, y = length(all_genres) - 12,
      label = cap
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 11,
    lineheight = 0.4,
    hjust = 0,
    halign = 0,
    vjust = 1,
    valign = 1,
    width = unit(3, "inch"),
    box.colour = "transparent",
    fill = "transparent"
  ) +
  scale_y_discrete(position = "right") +
  scale_fill_gradient(
    na.value = na_col,
    high = dark_col,
    low = highlight_col,
    transform = "log10",
    name = "Number of movies"
  ) +
  theme_void(base_family = body_font, base_size = 22) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.3),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      colour = text_col
    ),
    axis.text.y = element_text(
      hjust = 0,
      colour = text_col
    ),
    legend.title = element_text(
      hjust = 0,
      colour = text_col
    ),
    legend.text = element_text(
      margin = margin(l = 3),
      colour = text_col
    )
  )


# Save image --------------------------------------------------------------

ggsave(
  file.path("2024", "2024-07-30", paste0("20240730", ".png")),
  width = 6,
  height = 6
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-07-30", paste0("20240730", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
