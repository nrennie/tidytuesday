# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(PrettyCols)
library(ggiraph)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-09-02")
frogID_data <- tuesdata$frogID_data
frog_names <- tuesdata$frog_names


# Load fonts --------------------------------------------------------------

font_add_google("Bangers")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "#D6FFE7"
text_col <- "#013C1B"
highlight_col <- "#013C1B"

body_font <- "Ubuntu"
title_font <- "Bangers"


# Function ----------------------------------------------------------------

arc_bar_to_polygon <- function(start, end, r0, r, n = 50) {
  angles_outer <- seq(start, end, length.out = n)
  angles_inner <- seq(end, start, length.out = n)
  x <- c(r * cos(angles_outer), r0 * cos(angles_inner))
  y <- c(r * sin(angles_outer), r0 * sin(angles_inner))
  data.frame(x = x, y = y)
}


# Data wrangling ----------------------------------------------------------

frog_family <- frog_names |>
  mutate(
    across(c(subfamily, tribe), ~ str_trim(.x))
  ) |>
  separate_wider_delim(
    cols = scientificName,
    delim = " ",
    names = c("scientific1", "scientific2", "scientific3"),
    too_few = "align_start"
  )

frog_data <- frog_family |>
  drop_na(scientific2) |>
  select(subfamily, tribe, scientific1, scientific2) |>
  mutate(
    tribe = paste(subfamily, "\n", tribe),
    scientific1 = paste(tribe, "\n", scientific1),
    scientific2 = paste(scientific1, scientific2)
  ) |>
  distinct()

subfamily_data <- frog_data |>
  summarise(n = n(), .by = subfamily) |>
  mutate(category = subfamily) |>
  mutate(
    fraction = n / sum(n),
    end = cumsum(fraction) * 2 * pi,
    start = lag(end, default = 0),
    r0 = 0.2,
    r = 0.4
  )

tribe_data <- frog_data |>
  group_by(tribe) |>
  mutate(n = n()) |>
  ungroup() |>
  select(subfamily, tribe, n) |>
  distinct() |>
  rename(category = tribe) |>
  mutate(
    fraction = n / sum(n),
    end = cumsum(fraction) * 2 * pi,
    start = lag(end, default = 0),
    r0 = 0.4,
    r = 0.6
  )

sc1_data <- frog_data |>
  group_by(scientific1) |>
  mutate(n = n()) |>
  ungroup() |>
  select(subfamily, scientific1, n) |>
  distinct() |>
  rename(category = scientific1) |>
  mutate(
    fraction = n / sum(n),
    end = cumsum(fraction) * 2 * pi,
    start = lag(end, default = 0),
    r0 = 0.6,
    r = 0.8
  )

sc2_data <- frog_data |>
  group_by(scientific2) |>
  mutate(n = n()) |>
  ungroup() |>
  select(subfamily, scientific2, n) |>
  distinct() |>
  rename(category = scientific2) |>
  mutate(
    fraction = n / sum(n),
    end = cumsum(fraction) * 2 * pi,
    start = lag(end, default = 0),
    r0 = 0.8,
    r = 1.0
  )

all_data <- bind_rows(subfamily_data, tribe_data, sc1_data, sc2_data)
plot_data <- do.call(rbind, lapply(1:nrow(all_data), function(i) {
  poly <- arc_bar_to_polygon(all_data$start[i], all_data$end[i], all_data$r0[i], all_data$r[i])
  poly$category <- all_data$category[i]
  poly$subfamily <- all_data$subfamily[i]
  poly$n <- all_data$n[i]
  poly
}))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-02", "recording"),
  device = "png",
  width = 6,
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
title <- "Australian Frogs"
st <- "Australia is home to a unique and diverse array of frog species found almost nowhere else on Earth, with 257 native species distributed throughout the continent."
cap <- paste0(
  "**Data**: Australian Society of Herpetologists Official List of Australian Species. 2025.<br>**Graphic**: ", social, "<br>**Image**: @scarlettweiss (Unsplash)"
)


# Plot --------------------------------------------------------------------

g <- ggplot(
  data = plot_data,
  mapping = aes(
    x = x, y = y, group = category,
    fill = subfamily,
    tooltip = category,
    data_id = category
  )
) +
  geom_polygon_interactive(colour = bg_col, linewidth = 0.3) +
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 0,
      y = 0,
      image = "2025/2025-09-02/frog.png"
    ),
    size = 0.17
  ) +
  scale_fill_pretty_d("Dark") +
  labs(title = title, subtitle = st, caption = cap) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.8)
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
      margin = margin(b = 5, t = 10),
      family = body_font
    )
  )
g


# Save image --------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-09-02", paste0("20250902_static", ".png")),
  width = 6,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2025", "2025-09-02", paste0("20250902", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

gg_stop_recording()


# Interactive version -----------------------------------------------------

girafe(
  ggobj = g,
  width_svg = 6,
  height_svg = 7,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_hover(css = "fill:#e6b400;"),
    opts_tooltip(
      delay_mouseout = 1000,
      css = "padding: 5pt;
             font-family: Ubuntu;
             font-size: 1em;
             color: #013C1B;
             background-color: #D6FFE7;"
    )
  )
)

