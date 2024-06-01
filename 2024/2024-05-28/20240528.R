# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-05-28")
harvest_2020 <- tuesdata$harvest_2020
harvest_2021 <- tuesdata$harvest_2021
planting_2020 <- tuesdata$planting_2020
planting_2021 <- tuesdata$planting_2021
spending_2020 <- tuesdata$spending_2020
spending_2021 <- tuesdata$spending_2021


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#EDF4ED"
mid_col <- "#ABD1B5"
text_col <- "#51291E"
highlight_col <- "#79B791"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

spending <- list(spending_2020, spending_2021)
names(spending) <- c("2020", "2021")
spending_data <- spending |>
  bind_rows(.id = "year") |>
  group_by(year, vegetable) |>
  summarise(cost = sum(price_with_tax)) |>
  ungroup() |> 
  filter(
    !(vegetable %in% c(
      "enriched topsoil", "raised garden blend", 
      "dirt", "potting soil", "straw"))
  )

total_spend <- spending_data |>
  group_by(vegetable) |>
  summarise(cost = sum(cost)) |>
  ungroup() |>
  arrange(desc(cost)) |>
  slice_head(n = 10)

spend_order <- total_spend |>
  pull(vegetable) |>
  rev()

plot_data <- spending_data |>
  filter(vegetable %in% spend_order) |>
  complete(year, vegetable) |>
  mutate(vegetable = factor(vegetable, levels = spend_order))

spend_labels <- total_spend |>
  mutate(vegetable = factor(vegetable, levels = spend_order)) |>
  mutate(cost_lab = glue("**{str_to_title(vegetable)}**<br>${round(cost, 2)}"))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-05-28", "recording"),
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
  font_family = body_font
)
title <- glue("<span style='font-size:54pt;font-family:{title_font};'>**Highest spending on vegetables**</span><br>")
st <- "The {gardenR} package contains data collected by Lisa Lendway from her
vegetable garden, for the summers of 2020 and 2021. This chart shows the 10 vegetables with the highest 
total spend across both years."
tag <- paste0(
  title, st
)
cap <- paste0(
  "**Data**: {gardenR}<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

dodge <- 2.2

ggplot() +
  # bar charts
  geom_rect(
    data = filter(plot_data, year == 2020),
    mapping = aes(
      xmin = -1 * dodge, xmax = (-1 * dodge) - cost,
      ymin = as.numeric(vegetable) - 0.45,
      ymax = as.numeric(vegetable) + 0.45
    ),
    fill = mid_col,
    colour = highlight_col,
  ) +
  geom_rect(
    data = filter(plot_data, year == 2021),
    mapping = aes(
      xmin = dodge, xmax = dodge + cost,
      ymin = as.numeric(vegetable) - 0.45,
      ymax = as.numeric(vegetable) + 0.45
    ),
    fill = mid_col,
    colour = highlight_col,
  ) +
  # year money labels
  ggtext::geom_textbox(
    data = drop_na(plot_data),
    mapping = aes(
      x = case_when(
        year == 2020 ~ (-1 * dodge) - cost,
        TRUE ~ dodge + cost
      ),
      y = as.numeric(vegetable),
      label = glue("${round(cost, 2)}"),
      hjust = case_when(
        year == 2021 ~ 1,
        TRUE ~ 0
      ),
      halign = case_when(
        year == 2021 ~ 1,
        TRUE ~ 0
      )
    ),
    family = body_font,
    size = 8,
    fill = NA,
    colour = text_col,
    lineheight = 0.45,
    box.colour = NA
  ) +
  # money labels
  ggtext::geom_textbox(
    data = spend_labels,
    mapping = aes(
      x = 0,
      y = as.numeric(vegetable),
      label = cost_lab
    ),
    family = body_font,
    size = 8.5,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    colour = text_col,
    lineheight = 0.45,
    box.colour = NA
  ) +
  # year labels
  annotate(
    "text",
    x = -1 * dodge, y = 11, label = "2020",
    family = title_font,
    size = 14,
    hjust = 1,
    fontface = "bold",
    colour = text_col
  ) +
  annotate(
    "text",
    x = dodge, y = 11, label = "2021",
    family = title_font,
    size = 14,
    hjust = 0,
    fontface = "bold",
    colour = text_col
  ) +
  scale_x_continuous(limits = c(-26, 26)) +
  scale_y_continuous(limits = c(0.5, 11.5)) +
  coord_cartesian(expand = FALSE) +
  # text
  labs(tag = tag, caption = cap) +
  # styling
  theme_void(base_family = body_font, base_size = 28) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(1, 0.3),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font,
      maxwidth = 0.6,
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-05-28", paste0("20240528", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
