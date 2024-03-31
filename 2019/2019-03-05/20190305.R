# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)


# Load data ---------------------------------------------------------------

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey95"
text_col <- "black"
highlight_col <- "#93032E"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

set.seed(123)
plot_data <- jobs_gender |>
  filter(year == 2016) |>
  select(occupation, major_category, total_earnings_male, total_earnings_female) |>
  drop_na() |>
  group_by(major_category) |>
  mutate(
    min_cat = min(total_earnings_female),
    max_cat = max(total_earnings_female)
  ) |>
  slice_sample(n = 1) |>
  ungroup() |>
  mutate(
    label = glue("{str_to_sentence(occupation)} ({major_category})")
  ) |>
  mutate(label = reorder(label, -total_earnings_female))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 7.5,
  height = 5.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "The Gender Pay Gap"
st <- "The gender pay gap is the gap between what men and women are paid. Most
commonly, it refers to the median annual pay of all women who work full time
and year-round, compared to the pay of a similar cohort of men. This chart shows
the median annual income of female employess for a random sample of 8 occupations -
one from each major occupation category. The black line shows the median annual
income of males in the same occupation. The background bars indicate the minimum and maximum
median female incomes for other occupations in the same major category."
cap <- paste0(
  "**Data**: Bureau of Labor Statistics (2016)<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_col(
    mapping = aes(x = max_cat, y = "1"),
    fill = alpha(highlight_col, 0.3),
    width = 1
  ) +
  geom_col(
    mapping = aes(x = min_cat, y = 1),
    fill = alpha(highlight_col, 0.3),
    width = 1
  ) +
  geom_col(
    mapping = aes(x = total_earnings_female, y = 1),
    width = 0.35,
    fill = highlight_col
  ) +
  geom_linerange(
    aes(
      x = total_earnings_male,
      ymin = 1 - 0.3,
      ymax = 1 + 0.3
    ),
    linewidth = 1.5,
    colour = text_col
  ) +
  facet_wrap(~label,
    ncol = 2, strip.position = "left",
    labeller = labeller(label = label_wrap_gen(28))
  ) +
  scale_x_continuous(labels = comma) +
  labs(
    title = title, subtitle = st, caption = cap,
    x = "Median annual income in 2016 ($)", y = ""
  ) +
  theme_minimal(base_size = 24, base_family = body_font) +
  theme(
    plot.margin = margin(5, 15, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(2)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(
      hjust = 1,
      angle = 0,
      lineheight = 0.4,
      margin = margin(r = 0, l = 5)
    ),
    panel.spacing.y = unit(0.4, "lines"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.x = element_line(
      linewidth = 0.5,
      colour = alpha(text_col, 0.1)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", paste0("day_01", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
