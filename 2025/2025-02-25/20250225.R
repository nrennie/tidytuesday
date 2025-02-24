# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(rcartocolor)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-02-25")
article_dat <- tuesdata$article_dat
model_dat <- tuesdata$model_dat


# Load fonts --------------------------------------------------------------

font_add_google("Merriweather Sans", "merriweather")
showtext_auto()
showtext_opts(dpi = 300)


# Data wrangling ----------------------------------------------------------

study_data <- article_dat |>
  mutate(study_type = if_else(study_type != "RCT", str_to_lower(study_type), "randomised controlled trial")) |>
  group_by(year, study_type) |>
  summarise(count = n()) |>
  mutate(
    year.count = sum(count),
    prop = count / sum(count)
  ) |>
  ungroup() |>
  mutate(year = factor(year)) |>
  drop_na()

study_levels <- study_data |> 
  summarise(count = sum(count), .by = study_type) |> 
  arrange(count) |> 
  pull(study_type)
  
plot_data <- study_data |> 
  mutate(
    study_type = factor(study_type, levels = study_levels)
  )

# Define colours and fonts-------------------------------------------------

col_palette <- rcartocolor::carto_pal(n = length(study_levels)+1, name = "Vivid")[1:length(study_levels)]
names(col_palette) <- study_levels

bg_col <- "#002D3D"
text_col <- "white"
highlight_col <- col_palette[4]

body_font <- "merriweather"
title_font <- "merriweather"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-02-25", "recording"),
  device = "png",
  width = 9,
  height = 4.5,
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
st <- glue("<span style='font-size: 22pt;'>**How are racial and ethnic disparities studied?**</span><br><br>A variety of study types have been used to investigate racial and ethnic disparities in reproductive medicine, as published in the eight highest impact peer-reviewed Ob/Gyn journals from January 1, 2010 through June 30, 2023.<br><br>The number of studies is generally increasing over time. The most common study type is a <span style='color: {col_palette[6]}'>**{names(col_palette[6])}**</span>, followed by a <span style='color: {col_palette[5]}'>**{names(col_palette[5])}**</span>, <span style='color: {col_palette[4]}'>**{names(col_palette[4])}**</span>, <span style='color: {col_palette[3]}'>**{names(col_palette[3])}**</span>, then <span style='color: {col_palette[2]}'>**{names(col_palette[2])}**</span>. A <span style='color: {col_palette[1]}'>**{names(col_palette[1])}**</span> is the least common study type, but became more common in papers published during the COVID-19 pandemic.")
cap <- paste0(
  "**Data**: doi: 10.1016/j.ajog.2024.07.024 | **Graphic**:", social
)


# Plot --------------------------------------------------------------------

base_plot <- ggplot(
  data = plot_data
) +
  geom_col(
    mapping = aes(
      x = year,
      y = prop,
      width = year.count,
      fill = study_type
    ),
    position = "fill",
    colour = bg_col,
    linewidth = 0.5
  ) +
  facet_grid(year~"1", scales = "free_y", space = "free_y",
             switch = "y") +
  scale_fill_manual(
    values = col_palette
  ) +
  scale_y_continuous(expand = expansion(0, 0)) +
  coord_flip()

# chart version
base_plot + 
  labs(tag = st,
       caption = cap) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 5, 300),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_text(
      colour = text_col,
      size = 7,
      margin = margin(r = 2, b = 1, t = 1)
    ),
    plot.tag.position = c(-0.85, 0.95),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      vjust = 1,
      margin = margin(b = 10, t = 0),
      lineheight = 1.2,
      family = body_font,
      maxwidth = 0.9,
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 0, t = 5, r = -5),
      lineheight = 0.5,
      family = body_font
    )
  )

ggsave(
  filename = file.path("2025", "2025-02-25", paste0("20250225", ".png")),
  height = 4.5,
  width = 9,
  bg = bg_col,
  units = "in",
  dpi = 300
)

# data art version
base_plot +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave(
  filename = file.path("2025", "2025-02-25", paste0("20250225_art", ".png")),
  height = 4.5,
  width = 4.5,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2025", "2025-02-25", paste0("20250225", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
