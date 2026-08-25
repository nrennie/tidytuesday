# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(statebins)
library(ggnewscale)
library(tidytext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-08-25")
country_lyrics <- tuesdata$country_lyrics
top_all_writers <- tuesdata$top_all_writers
top_primary_writers <- tuesdata$top_primary_writers
top_producers <- tuesdata$top_producers


# Load fonts --------------------------------------------------------------

font_add_google("Orbitron")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Orbitron"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"


# Data wrangling ----------------------------------------------------------

# Explore common words
data(stop_words)
all_words <- country_lyrics |>
  select(song, lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  distinct() |>
  anti_join(stop_words, by = "word") |>
  count(word) |>
  arrange(-n)

# Choose colours
cols_in <- colors()[colors() %in% unique(all_words$word)]
all_cols <- country_lyrics |>
  select(year = entered_top_30_in, lyrics) |>
  mutate(
    lyrics = str_to_lower(lyrics),
    as_tibble(
      outer(lyrics, cols_in, \(x, y) str_detect(x, fixed(y))),
      .name_repair = ~cols_in
    )
  ) |>
  pivot_longer(
    cols = -c(year, lyrics),
    values_to = "value",
    names_to = "topic"
  ) |>
  filter(value) |>
  count(topic) |>
  arrange(-n) |>
  filter(n >= 20) |>
  pull(topic)

plot_data <- country_lyrics |>
  select(year = entered_top_30_in, lyrics) |>
  mutate(
    lyrics = str_to_lower(lyrics),
    as_tibble(
      outer(lyrics, all_cols, \(x, y) str_detect(x, fixed(y))),
      .name_repair = ~all_cols
    )
  ) |>
  pivot_longer(
    cols = -c(year, lyrics),
    values_to = "value",
    names_to = "topic"
  ) |>
  filter(value) |>
  group_by(year, topic) |>
  mutate(y = row_number() + 0.75) |>
  ungroup()

max_rows <- ceiling(1 / 5 * (plot_data |>
  count(year, topic) |>
  slice_max(n) |>
  pull(n))) * 5

bg_data <- expand.grid(
  year = sort(unique(plot_data$year)),
  y = 1:max_rows
) |>
  as_tibble()

plot_data$topic <- factor(plot_data$topic,
  levels = all_cols,
  labels = str_to_title(all_cols)
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The Colour Palette of Country Music"
st <- "The number of songs in the Top 30 Billboard Country Airplay with lyrics that mention..."
cap <- source_caption(source = "youtube.com/@GradySmith", graphic = social, sep = " | ")


# Plot --------------------------------------------------------------------

ggplot() +
  statebins:::geom_rtile(
    data = bg_data,
    mapping = aes(x = year, y = y, fill = y),
    alpha = 0.1,
    radius = grid::unit(2.2, "pt"),
    width = 0.95,
    height = 0.85
  ) +
  scale_fill_gradientn(colours = PrettyCols::prettycols("Ocean")[2:5]) +
  new_scale_fill() +
  # top
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = year, y = y, fill = topic),
    radius = grid::unit(2.2, "pt"),
    width = 0.95,
    height = 0.85
  ) +
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = year, y = y, alpha = -y),
    fill = bg_col,
    radius = grid::unit(2.2, "pt"),
    width = 0.95,
    height = 0.85
  ) +
  # bottom
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = year, y = -y, fill = topic),
    alpha = 0.1,
    radius = grid::unit(2.2, "pt"),
    width = 0.95,
    height = 0.85
  ) +
  statebins:::geom_rtile(
    data = plot_data,
    mapping = aes(x = year, y = -y, alpha = -y),
    fill = bg_col,
    radius = grid::unit(2.2, "pt"),
    width = 0.95,
    height = 0.85
  ) +
  scale_alpha(range = c(0, 0.9)) +
  scale_fill_identity() +
  geom_text(
    data = plot_data |> select(year) |> distinct(),
    mapping = aes(x = year, y = 0, label = year),
    size = 2.3,
    family = body_font,
    colour = text_col
  ) +
  geom_text(
    data = plot_data |> count(year, topic),
    mapping = aes(x = year, y = n + 0.75 + 1, label = n),
    vjust = 0,
    size = 3,
    family = body_font,
    colour = text_col,
    fontface = "bold"
  ) +
  facet_wrap(~topic, ncol = 3) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = -40),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 5, b = 5),
      size = rel(0.9),
      colour = text_col,
      family = title_font
    ),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0, "lines"),
    strip.clip = "off"
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-08-25", paste0("20260825", ".png"))
)
