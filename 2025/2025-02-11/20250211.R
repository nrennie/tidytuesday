# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(sf)
library(rcartocolor)
library(VoronoiPlus)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-02-11")
cdc_datasets <- tuesdata$cdc_datasets
fpi_codes <- tuesdata$fpi_codes
omb_codes <- tuesdata$omb_codes


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "black"
highlight_col <- "#1c68bc"

body_font <- "roboto"
title_font <- "robotoslab"


# Data wrangling ----------------------------------------------------------

code_lookup <- fpi_codes |>
  filter(agency_name == "Department of Health and Human Services") |>
  select(program_name, program_code) |>
  mutate(program_code = str_replace(program_code, "-", ":"))

prog_data <- cdc_datasets |>
  filter(program_code %in% code_lookup$program_code) |>
  select(program_code, category) |>
  left_join(code_lookup, by = "program_code") |>
  mutate(
    category = if_else(
      category == "This dataset has not been categorized",
      "Uncategorized",
      category
    )
  ) |>
  drop_na() |>
  count(program_name, category)

prog_list <- prog_data |> 
  summarise(
    n = sum(n),
    .by = program_name
  ) |> 
  arrange(-n) |> 
  slice_head(n = 9) |> 
  pull(program_name)

plot_data <- prog_data |> 
  mutate(
    program_name = case_when(
      program_name %in% prog_list ~ program_name,
      TRUE ~ "Other"
    )
  ) |> 
  group_by(program_name, category) |> 
  summarise(n = sum(n)) |> 
  ungroup()


vor_data <- voronoi_treemap(n ~ program_name + category, data = plot_data)
set.seed(1234)
groups <- filter(vor_data, level == 1)
subgroups <- filter(vor_data, level == 2) |>
  mutate(grp = paste(group, parent)) |> 
  group_by(grp) |>
  mutate(alpha = runif(1, 0, 0.6)) |>
  ungroup()

cat_labels <- groups |>
  select(group, x, y) |>
  st_as_sf(coords = c("x", "y")) |>
  group_by(group) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON") |>
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) |>
  st_drop_geometry()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-02-11", "recording"),
  device = "png",
  width = 5,
  height = 7,
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
title <- "The Defense Against Disappearing Data"
st <- "The Trump administration has ordered agencies to purge their websites of any references to topics such as LGBTQ+ rights. An effort is underway to back up this publicly funded data before it is lost.<br>The following Voronoi diagram shows the categories applied to datasets from the Centers for Disease Control and Prevention, associated with different programs from the Department of Health and Human Services. The areas represent the number of datasets that have been backed up."
cap <- paste0(
  st, "<br><br>**Data**: archive.org (28 Jan 2025)<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = group, fill = group),
    colour = text_col,
    linewidth = 0.9
  ) + 
  geom_polygon(
    data = subgroups,
    mapping = aes(x = x, y = y, group = grp, alpha = alpha),
    fill = bg_col,
    colour = text_col,
    linewidth = 0.3
  ) +
  geom_polygon(
    data = groups,
    mapping = aes(x = x, y = y, group = group),
    colour = "transparent",
    fill = bg_col,
    alpha = 0.5,
    linewidth = 0.9
  ) +
  geom_textbox(
    data = cat_labels,
    mapping = aes(x = x, y = y, label = group),
    colour = text_col,
    family = title_font,
    fontface = "bold",
    size = 2.4,
    fill = "transparent",
    box.colour = "transparent",
    maxwidth = 0.23,
    hjust = 0.5,
    halign = 0.5
  ) +
  scale_alpha_identity() +
  scale_fill_carto_d(
    palette = "Bold"
  ) +
  coord_fixed() +
  labs(
    title = title,
    subtitle = cap
  ) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-02-11", paste0("20250211", ".png")),
  height = 7,
  width = 5,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-02-11", paste0("20250211", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
