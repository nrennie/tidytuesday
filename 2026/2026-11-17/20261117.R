# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-11-17")
cholesterol_mortality <- tuesdata$cholesterol_mortality


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"
comp_col <- "#197176"

# Data wrangling ----------------------------------------------------------

summary_data <- cholesterol_mortality |>
  # select(age, sex, race, tc_hdl_ratio) |>
  mutate(age_group = case_when(
    age >= 18 & age < 30 ~ "Under 30",
    age >= 30 & age < 40 ~ "30 - 39",
    age >= 40 & age < 50 ~ "40 - 49",
    age >= 50 & age < 60 ~ "50 - 59",
    age >= 60 & age < 70 ~ "60 - 69",
    age >= 70 & age < 80 ~ "70 - 79",
    age >= 80 ~ "80 and over"
  )) |>
  mutate(
    age_group = factor(age_group, levels = c(
      "Under 30", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80 and over"
    ))
  ) |>
  group_by(age_group, sex, race) |>
  summarise(mean_ratio = mean(tc_hdl_ratio, na.rm = TRUE)) |>
  ungroup() |>
  rowwise() |>
  mutate(
    x = case_when(
      sex == "Female" ~ list(c(-sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2)),
      sex == "Male" ~ list(c(-sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2))
    ),
    y = case_when(
      sex == "Female" ~ list(c(-sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2)),
      sex == "Male" ~ list(c(-sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, sqrt(mean_ratio) / 2, -sqrt(mean_ratio) / 2))
    )
  ) |>
  ungroup()

race_levels <- summary_data |>  
  filter(age_group == "Under 30", sex == "Male") |> 
  arrange(mean_ratio) |> 
  pull(race)

summary_data$race <- factor(summary_data$race, levels = race_levels)

plot_data <- summary_data |>
  unnest(c(x, y))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Women have a more favourable Total Cholesterol-to-HDL ratio than men"
st <- "The ratio of total cholesterol to HDL (high-density lipoprotein, the *good* cholesterol) is a common clinical risk measure for cardiovascular disease, where lower is generally considered better. The ratio is typically lower for men, for those under the age of 40 or over 70, and for those who are non-Hispanic Black.<br><br>The size of the triangles represents the ratio for <span style='color:#114b4f;'>**male**</span> (upper left) and <span style='color:#6a044f;'>**female**</span> (lower right) survey respondents, i.e. lower risk subgroups are represented by smaller triangles. The dark gap around each triangle represents the reduction in risk compared to the most at-risk subgroup (other Hispanic men aged 40-49)."
cap <- source_caption(source = "National Health and Nutrition Examination Survey", graphic = social)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  annotate(
    "rect",
    xmin = -max(plot_data$x, na.rm = TRUE), xmax = max(plot_data$x, na.rm = TRUE),
    ymin = -max(plot_data$y, na.rm = TRUE), ymax = max(plot_data$y, na.rm = TRUE),
    fill = text_col,
    colour = text_col
  ) +
  geom_polygon(
    fill = "white"
  ) +
  geom_polygon(
    mapping = aes(fill = sex),
    alpha = 0.5
  ) +
  geom_text(
    data = summary_data,
    mapping = aes(
      x = c(-0.5, 0.5)[as.numeric(sex == "Female") + 1],
      y = c(0.5, -0.5)[as.numeric(sex == "Female") + 1],
      colour = sex, label = sprintf("%.1f", mean_ratio)
    ),
    family = body_font,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = -max(plot_data$x, na.rm = TRUE), xend = max(plot_data$x, na.rm = TRUE),
    y = -max(plot_data$y, na.rm = TRUE), yend = max(plot_data$y, na.rm = TRUE),
    colour = bg_col
  ) +
  facet_grid(race ~ age_group, switch = "y") +
  scale_fill_manual(
    values = c("Female" = highlight_col, "Male" = comp_col)
  ) +
  scale_colour_manual(
    values = c("Female" = "#6a044f", "Male" = "#114b4f")
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 11, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = text_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 15),
      family = body_font
    ),
    strip.text.x.top = element_text(
      face = "bold",
      size = rel(0.9),
      margin = margin(b = 3)
    ),
    strip.text.y.left = element_text(
      face = "bold",
      hjust = 1,
      margin = margin(r = 3),
      size = rel(0.9)
    ),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1,
    strip.clip = "off"
  ) +
  canvas(
    width = 7, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-11-17", paste0("20261117", ".png"))
)
