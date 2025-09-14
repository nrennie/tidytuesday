# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggtern)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-16")
all_recipes <- tuesdata$all_recipes
cuisines <- tuesdata$cuisines


# Load fonts --------------------------------------------------------------

font_add_google("Space Grotesk", "space")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

col_palette <- PrettyCols::PrettyColsPalettes[["Dark"]][[1]][2:4]
bg_col <- "grey99"
text_col <- "#1F102E"
highlight_col <- col_palette[1]

body_font <- "space"
title_font <- "space"


# Data wrangling ----------------------------------------------------------

plot_data <- cuisines |>
  filter(country %in% c("French", "Italian", "Japanese", "Lebanese", "Greek", "Cuban")) |>
  select(fat, carbs, protein, country) |>
  mutate(id = row_number()) |>
  pivot_longer(-c(country, id)) |>
  group_by(id) |>
  mutate(
    total = sum(value),
    perc = 100 * value / total
  ) |>
  ungroup() |>
  select(country, id, name, perc) |>
  pivot_wider(values_from = "perc", names_from = "name") |>
  drop_na()

country_orders <- plot_data |> 
  group_by(country) |> 
  summarise(fat = max(fat)) |> 
  arrange(fat) |> 
  pull(country)
plot_data$country <- factor(plot_data$country, levels = country_orders)

regions <- data.frame(
  fat = c(
    1 / 3, 0.5, 1, 0.5, 1 / 3, 
    1 / 3, 0.5, 0, 0, 1 / 3, 
    1 / 3, 0, 0, 0.5, 1 / 3
  ),
  carbs = c(
    1 / 3, 0, 0, 0.5, 1 / 3,
    1 / 3, 0.5, 1, 0.5, 1 / 3,
    1 / 3, 0.5, 0, 0, 1 / 3
  ),
  protein = c(
    1 / 3, 0.5, 0, 0, 1 / 3,
    1 / 3, 0, 0, 0.5, 1 / 3,
    1 / 3, 0.5, 1, 0.5, 1 / 3
  ),
  region = c(rep("A", 5), rep("B", 5), rep("C", 5))
)

labels <- data.frame(
  fat = c(0, 0.5, 0.5),
  carbs = c(0.5, 0, 0.5),
  protein = c(0.5, 0.5, 0),
  label = c("\u2190 % Carbohydrates\n", "\n% Protein \u2192", "\u2190 % Fat\n"),
  angle = c(-60, 0, 60) 
)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-16", "recording"),
  device = "png",
  width = 8,
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
  mastodon = NA
)
title <- "There are fewer high fat Italian recipes compared to other cuisines"
st <- glue("Each point shows the percentage of each serving consisting of <span style='color:{col_palette[1]};'>**fat**</span>, <span style='color:{col_palette[2]};'>**carbohydrates**</span>, and <span style='color:{col_palette[3]};'>**protein**</span> by weight, for a different recipe.")
cap <- paste0(
  "**Data**: allrecipes.com | **Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggtern(
  data = plot_data,
  mapping = aes(x = fat, y = carbs, z = protein)
) +
  geom_polygon(
    data = regions,
    mapping = aes(fill = region), alpha = 0.3
  ) +
  geom_point(shape = 16, alpha = 0.8, colour = text_col, size = 2) +
  geom_text(
    data = labels,
    mapping = aes(label = label, angle = angle),
    family = body_font,
    colour = text_col
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  facet_wrap(~country) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = NULL, y = NULL, z = NULL
  ) +
  coord_tern(expand = FALSE) +
  theme_minimal(base_family = body_font) +
  theme_custom(base_size = 11, col.T = bg_col, col.L = bg_col, col.R = bg_col) +
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
      size = rel(1.45)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      family = body_font
    ),
    strip.text = element_text(face = "bold", colour = text_col,
                              size = rel(1.2)),
    strip.background = element_blank(),
    legend.position = "none"
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-09-16", paste0("20250916", ".png")),
  width = 8,
  height = 6,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-09-16", paste0("20250916", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
