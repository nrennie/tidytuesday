# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(treemapify)
library(ggh4x)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-03-18")
palmtrees <- tuesdata$palmtrees


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "black"
highlight_col <- "#F6511D"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

plot_data <- palmtrees |>
  select(main_fruit_colors, conspicuousness) |>
  group_by(conspicuousness) |>
  mutate(n = n()) |>
  ungroup() |>
  drop_na() |>
  separate_longer_delim(main_fruit_colors, delim = "; ") |>
  group_by(main_fruit_colors, conspicuousness) |>
  mutate(n_col = n()) |>
  ungroup() |>
  distinct()

col_palette <- c("black", "#3F7CAC", "#573D1C", "#F5E2CC", "#90A959", "grey55", "#F4EBD9", "#F6511D", "#CE4760", "#521945", "#A2102B", "#EBC4AD", "white", "#F7B32B")
names(col_palette) <- sort(unique(plot_data$main_fruit_colors))

prop_data <- plot_data |>
  select(conspicuousness, n) |>
  distinct() |>
  arrange(conspicuousness) |>
  mutate(prop = n / n[1])


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-03-18", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- "Palm Trees"
st <- "Plant traits are critical to plant form and function - including growth, survival and reproduction - and therefore shape fundamental aspects of population and ecosystem dynamics as well as ecosystem services. Palms (Arecaceae) are a plant family with keystone importance in tropical and subtropical ecosystems."
cap <- paste0(
  "**Data**: {palmtrees} | **Graphic**:", social
)

grp_names <- c(
  `conspicuous` = glue("**Conspicuous**<br>{prop_data$n[1]} species<br>*Palm tree species with conspicuous colours, typically have fruit that is orange, red, yellow, or pink.*"),
  `cryptic` = glue("**Cryptic**<br>{formatC(prop_data$n[2], big.mark=',')} species<br>*Palm trees with cryptic colours, typically have fruit that is brown, black, green, blue, cream, ivory, or grey.*")
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(
    area = n_col, fill = main_fruit_colors
  )
) +
  geom_treemap(colour = text_col) +
  facet_manual(~conspicuousness,
    design = "AB",
    widths = c(prop_data$prop),
    heights = c(prop_data$prop),
    labeller = as_labeller(grp_names)
  ) +
  scale_fill_manual(values = col_palette) +
  labs(title = title, subtitle = st, caption = cap) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 8, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    strip.text = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      minheight = 0.1
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-03-18", paste0("20250318", ".png")),
  height = 5,
  width = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-03-18", paste0("20250318", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
