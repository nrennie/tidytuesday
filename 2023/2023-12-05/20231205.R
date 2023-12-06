# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(pals)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-05")
life_expectancy <- tuesdata$life_expectancy
life_expectancy_different_ages <- tuesdata$life_expectancy_different_ages
life_expectancy_female_male <- tuesdata$life_expectancy_female_male


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey15"
text_col <- "grey90"
highlight_col <- "#23967F"

body_font <- "roboto"
title_font <- "roboto"


# Data wrangling ----------------------------------------------------------

plot_data <- life_expectancy |>
  filter(Year %in% seq(2011, 2021, by = 1)) |>
  group_by(Year) |>
  slice_max(LifeExpectancy, n = 8) |>
  arrange(Year, desc(LifeExpectancy)) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  mutate(rank = factor(rank, levels = 1:8))

label_data <- plot_data |> 
  group_by(Entity) |> 
  slice_max(Year) |> 
  ungroup()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-12-05", "recording"),
  device = "png",
  width = 8,
  height = 4.5,
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
title <- "Life Expectancy"
st <- "Across the world, people are living longer. In 1900, the average life 
expectancy of a newborn was 32 years. By 2021 this had more than doubled to 
71 years. Since 2011, Monaco and Hong Kong have consistently come out on top 
of the list of countries with the highest life expectancy."
cap <- paste0(
  "**Data**: Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------


ggplot() +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = Year,
      y = rank,
      colour = Entity
    ),
    size = 2.5
  ) +
  geom_line(
    data = plot_data,
    mapping = aes(
      x = Year,
      y = rank,
      colour = Entity,
      group = Entity
    )
  ) +
  geom_label(
    data = label_data,
    mapping = aes(
      x = Year,
      y = rank,
      label = stringr::str_flatten(rep(" ", 20)),
      colour = Entity
    ),
    fill = bg_col,
    label.padding = unit(0.15, "lines")
  ) +
  geom_text(
    data = label_data,
    mapping = aes(
      x = Year,
      y = rank,
      label = Entity,
      colour = Entity
    ),
    family = body_font
  ) +
  scale_x_continuous(expand = expansion(0, 0.1),
                     limits = c(2010.75, 2021.25),
                     breaks = 2011:2021,
                     minor_breaks = seq(2011, 2021, by = 0.25)) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values=unname(polychrome())) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_light(base_size = 27, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 10),
    axis.title = element_blank(),
    axis.text = element_text(colour = alpha(text_col, 0.8)),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    panel.grid.major.y = element_line(colour = alpha(text_col, 0.5), linewidth = 0.1),
    panel.grid.major.x = element_line(colour = alpha(text_col, 0.5), linewidth = 0.1),
    panel.grid.minor.x = element_line(colour = alpha(text_col, 0.2), linewidth = 0.1),
    panel.border = element_rect(colour = alpha(text_col, 0.7), fill = NA, linewidth = 0.1),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      face = "bold",
      size = 50,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
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
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-12-05", paste0("20231205", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
