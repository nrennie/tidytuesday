# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)
library(ggforce)
library(geomtextpath)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-12-30")
christmas_novel_authors <- tuesdata$christmas_novel_authors
christmas_novel_text <- tuesdata$christmas_novel_text
christmas_novels <- tuesdata$christmas_novels


# Load fonts --------------------------------------------------------------

font_add_google("DynaPuff")
font_add_google("Nunito")
font_add("FA",
         regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "DynaPuff"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F7F3"
text_col <- "#274029"
col_palette <- c("#315C2B", "#710627", "#1F2A44")


# Data wrangling ----------------------------------------------------------

unique_novels <- christmas_novels |>
  group_by(gutenberg_id) |>
  slice_head(n = 1) |>
  ungroup()

set.seed(20251225)
plot_data <- unique_novels |>
  mutate(
    Santa = str_detect(title, "Santa"),
    Tree = str_detect(title, "\\bTree\\b|\\btree\\b"),
    Eve = str_detect(title, "\\bEve\\b"),
    Day = str_detect(title, "\\bDay\\b"),
    Present = str_detect(title, "\\bPresent\\b|\\bPresents\\b"),
    Story = str_detect(title, "\\bStory\\b|\\bstory\\b"),
    Carol = str_detect(title, "\\bCarol\\b")
  ) |>
  select(-c(1:3)) |>
  colSums() |>
  enframe() |>
  mutate(
    y = -value,
    x = row_number(),
    r = 0.66,
    col1 = sample(col_palette, size = 7, replace = TRUE),
    col2 = sample(col_palette, size = 7, replace = TRUE)
  )

snow_col_palette <- c("white", "#E6EFE8", "#CDDFD1")
n <- 30
x_range <- seq(0.5, 7.5, by = 0.1)
y_range <- seq(0, -12.5, by = -0.1)
snowflakes <- data.frame(
  x = sample(x_range, n),
  y = sample(y_range, n),
  col = sample(snow_col_palette, size = n, replace = TRUE),
  size = runif(n, 7, 14)
)
snowflakes2 <- data.frame(
  x = sample(x_range, n),
  y = sample(y_range, n),
  col = sample(snow_col_palette, size = n, replace = TRUE),
  size = runif(n, 7, 14)
)

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-12-30", "recording"),
  device = "png",
  width = 5,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- glue("<span style='font-family:{title_font}; font-size: 18pt;'>A Christmas Story</span><br><br>")
st <- glue("There are {nrow(unique_novels)} novels in Project Gutenberg that contain the word **Christmas** in the title. Other festive words that feature include present, carol, day, eve, tree, Santa, and **story**, which is listed in {filter(plot_data, name == 'Story')$value} titles.")
cap <- paste0(
  title, st, "<br><br>**Data**:  Project Gutenberg<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x0 = x, y0 = y, r = r)
) +
  geom_textbox(
    data = snowflakes,
    mapping = aes(
      x = x, y = y, 
      size = I(size), colour = I(col),
      label = "<span style='font-family:\"FA\";'>&#xf2dc;</span>"
    ),
    fill = "transparent",
    box.colour = "transparent",
    inherit.aes = FALSE
  ) +
  geom_segment(
    mapping = aes(
      x = x,
      y = y + r - 0.05, yend = 0
    ),
    colour = alpha(text_col, 0.8)
  ) +
  geom_rect(
    mapping = aes(
      xmin = x - 0.15, xmax = x + 0.15,
      ymin = y + r - 0.05, ymax = y + r + 0.15,
      fill = I(col2)
    ),
    colour = "transparent"
  ) +
  geom_circle(
    mapping = aes(fill = I(col1)),
    colour = NA
  ) +
  geom_textcurve(
    mapping = aes(
      x = x - r, xend = x + r, y = y, yend = y,
      label = str_to_upper(name)
    ),
    colour = bg_col,
    curvature = 0.3,
    linewidth = 1,
    size = 4.5,
    lineend = "round",
    family = title_font
  ) +
  geom_textbox(
    data = snowflakes2,
    mapping = aes(
      x = x, y = y, 
      size = I(size), colour = I(alpha(col, 0.4)),
      label = "<span style='font-family:\"FA\";'>&#xf2dc;</span>"
    ),
    fill = "transparent",
    box.colour = "transparent",
    inherit.aes = FALSE
  ) +
  labs(tags = cap) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = body_font) +
  theme(
    plot.margin = margin(0, 5, 15, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = body_font,
      width = 0.85
    ),
    plot.tag.position = c(0.03, 0.4)
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-12-30", paste0("20251230", ".png")),
  width = 5,
  height = 8,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-12-30", paste0("20251230", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
