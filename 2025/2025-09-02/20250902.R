# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggflowchart)
library(PrettyCols)
library(ggiraph)


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

level1 <- frog_family |>
  select(subfamily, tribe) |>
  distinct()
names(level1) <- c("from", "to")

level2 <- frog_family |>
  select(tribe, scientific1) |>
  distinct() |>
  drop_na()
names(level2) <- c("from", "to")

level3 <- frog_family |>
  select(scientific1, scientific2) |>
  distinct() |>
  drop_na() |>
  mutate(scientific2 = paste(scientific1, scientific2))
names(level3) <- c("from", "to")

edges_data <- dplyr::bind_rows(level1, level2, level3)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-02", "recording"),
  device = "png",
  width = 6,
  height = 18,
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
  "**Data**: Australian Society of Herpetologists Official List of Australian Species. 2025.<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

"%notin%" <- function(x, y) {
  !("%in%"(x, y))
}

node_data <- NULL
layout <- "tree"
linewidth <- 0.5
x_nudge <- 0.5
y_nudge <- 0.32

# define position of nodes
node_layout <- ggflowchart:::get_layout(
  data = edges_data,
  layout = layout,
  node_data = node_data
)
node_layout <- ggflowchart:::add_node_attr(
  node_layout = node_layout,
  node_data = node_data
)
# define edges of node rectangles
plot_nodes <- ggflowchart:::get_nodes(
  node_layout = node_layout,
  x_nudge = x_nudge,
  y_nudge = y_nudge
)
# check if labels exist as a column,
# if not, add it as a duplicate of name
if ("label" %notin% colnames(plot_nodes)) {
  plot_nodes <- dplyr::mutate(plot_nodes, label = name)
}
# define arrows
plot_edges <- ggflowchart:::get_edges(
  data = edges_data,
  plot_nodes = plot_nodes,
  node_layout = node_layout
)

# create the flowchart
ggplot() +
  geom_rect(
    data = plot_nodes,
    mapping = aes(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      colour = (ymin < 0)
    ),
    fill = "transparent"
  ) +
  scale_colour_manual(
    values = c(alpha(text_col, 0.3), "transparent")
  ) +
  # add text
  geom_text(
    data = plot_nodes,
    mapping = aes(
      x = x,
      y = y + 0.29,
      label = label
    ),
    family = body_font,
    size = 2.3,
    colour = text_col,
    hjust = 0
  ) +
  # add arrows
  geom_path(
    data = plot_edges,
    mapping = aes(
      x = x,
      y = y,
      group = id
    ),
    arrow = arrow(
      length = unit(0.15, "cm"),
      type = "closed"
    ),
    colour = text_col
  ) +
  labs(title = title, subtitle = st, caption = cap) +
  coord_flip(clip = "off") +
  scale_x_continuous(expand = expansion(c(0, 0))) +
  scale_y_reverse(limits = c(NA, -0.8)) +
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
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 5),
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

ggsave(
  filename = file.path("2025", "2025-09-02", paste0("20250902_long", ".png")),
  width = 6,
  height = 18,
  bg = bg_col,
  units = "in",
  dpi = 300
)



# Sunburst plot -----------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-09-02", "recording"),
  device = "png",
  width = 6,
  height = 7,
  units = "in",
  dpi = 300
)

# Function
arc_bar_to_polygon <- function(start, end, r0, r, n = 50) {
  angles_outer <- seq(start, end, length.out = n)
  angles_inner <- seq(end, start, length.out = n)
  x <- c(r * cos(angles_outer), r0 * cos(angles_inner))
  y <- c(r * sin(angles_outer), r0 * sin(angles_inner))
  data.frame(x = x, y = y)
}

# Different data prep
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
polys <- do.call(rbind, lapply(1:nrow(all_data), function(i) {
  poly <- arc_bar_to_polygon(all_data$start[i], all_data$end[i], all_data$r0[i], all_data$r[i])
  poly$category <- all_data$category[i]
  poly$subfamily <- all_data$subfamily[i]
  poly$n <- all_data$n[i]
  poly
}))

# Plot
g <- ggplot(polys, aes(
  x = x, y = y, group = category,
  fill = subfamily,
  tooltip = category,
  data_id = category
)) +
  geom_polygon_interactive(colour = bg_col, linewidth = 0.3) +
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

ggsave(
  filename = file.path("2025", "2025-09-02", paste0("20250902_round", ".png")),
  width = 6,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)
gg_stop_recording()

# Interactive version
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


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2025", "2025-09-02", paste0("20250902", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
