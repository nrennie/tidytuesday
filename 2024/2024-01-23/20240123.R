# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-23")
english_education <- tuesdata$english_education


# Load fonts --------------------------------------------------------------

font_add_google("Open Sans", "open")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "grey20"
col_palette <- c("#871a5b", "#d3d3d3", "#206095")

body_font <- "open"
title_font <- "open"


# Data wrangling ----------------------------------------------------------

plot_data <- english_education |>
  filter(
    (size_flag %in% c("Medium Towns", "Small Towns", "Large Towns",
                      "Not BUA", "Other Small BUAs"))
  ) |> 
  select(
    income_flag, key_stage_2_attainment_school_year_2007_to_2008,
    key_stage_4_attainment_school_year_2012_to_2013, level_3_at_age_18
  ) |>
  filter(income_flag %in% c(
    "Higher deprivation towns",
    "Lower deprivation towns",
    "Mid deprivation towns"
  )) |>
  group_by(income_flag) |>
  summarise(
    ks2 = mean(key_stage_2_attainment_school_year_2007_to_2008),
    ks4 = mean(key_stage_4_attainment_school_year_2012_to_2013),
    lvl3 = mean(level_3_at_age_18)
  ) |>
  t() |>
  janitor::row_to_names(row_number = 1) |>
  as.data.frame() |>
  rownames_to_column(var = "level") |>
  as_tibble() |>
  mutate(across(-level, as.numeric)) |>
  mutate(across(-level, ~ .x - `Lower deprivation towns`)) |>
  pivot_longer(cols = -level)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-23", "recording"),
  device = "png",
  width = 7,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = col_palette[1],
  font_colour = text_col,
  font_family = body_font
)
title <- "The gap in educational attainment gets wider as young people progress 
through education"
st <- "Percentage point difference in the proportion of young people meeting 
specific attainment measures at age 11, 16 and 18 years, by town income deprivation 
groupings, England<br><br><br><br>0 = the attainment rate for those in low income deprivation towns"
cap <- paste0(
  "**Source**: Office for National Statistics<br>**Graphic**: inspired by ONS, recreated by ", social
)
label1 <- glue(
  'At age 11, the gap is {-1*round(filter(plot_data, level == "ks2", name == "Higher deprivation towns")$value)} percentage\npoints'
)
label2 <- glue(
  'By age 18, the gap reached {-1*round(filter(plot_data, level == "lvl3", name == "Higher deprivation towns")$value)}\npercentage points'
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_point(
    mapping = aes(
      x = level,
      y = value,
      fill = name
    ),
    pch = 21,
    colour = alpha(text_col, 0.7),
    size = 4
  ) +
  geom_text(
    data = data.frame(
      x = c(1.3, 2.5),
      y = c(-15, -21),
      label = c(label1, label2)
    ),
    mapping = aes(x = x, y = y, label = label),
    family = body_font,
    colour = text_col,
    size = 9,
    lineheight = 0.5
  ) +
  geom_curve(
    data = data.frame(x1 = c(1.3),
                      x2 = c(1.08),
                      y1 = c(-13),
                      y2 = c(-10)),
    mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.03, "npc")),
    colour = alpha(text_col, 0.9),
    curvature = 0.4
  ) +
  geom_curve(
    data = data.frame(x1 = c(2.5),
                      x2 = c(2.92),
                      y1 = c(-19),
                      y2 = c( -16)),
    mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.03, "npc")),
    colour = alpha(text_col, 0.9),
    curvature = -0.4
  ) +
  scale_fill_manual(
    values = 
      c("Higher deprivation towns" = col_palette[1],
        "Mid deprivation towns" = col_palette[2],
        "Lower deprivation towns" = col_palette[3]),
    breaks = c("Higher deprivation towns", "Mid deprivation towns", "Lower deprivation towns"),
    labels = c("Higher income deprivation", "Mid income deprivation", "Lower income deprivation"),
    guide = guide_legend(nrow = 1)
  ) +
  scale_x_discrete(
    labels = c("Key Stage 2", "Key Stage 4", "Level 3 at age 18"),
    expand = expansion(0, 0.2)
  ) +
  scale_y_continuous(
    breaks = seq(-25, 0, by = 5),
    minor_breaks = NULL,
    limits = c(-25, 0),
    expand = expansion(0, c(0, 0.5))
  ) +
  coord_cartesian(clip = "off") +
  labs(title = title, 
       subtitle = st,
       caption = cap) +
  theme_minimal(base_size = 32, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    legend.position = c(-0.1, 1.13),
    legend.justification = c(0, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1), hjust = 0, margin = margin(l = -10)),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.5),
      linewidth = 0.2,
      linetype = "dashed"
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.3),
      linewidth = 0.2
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      face = "bold",
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 20),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-23", paste0("20240123", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
