# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggstream)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-05-21")
emissions <- tuesdata$emissions


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Data wrangling ----------------------------------------------------------

plot_data <- emissions |>
  filter(commodity %in% c(
    "Sub-Bituminous Coal", "Metallurgical Coal", "Bituminous Coal",
    "Thermal Coal", "Anthracite Coal", "Lignite Coal"
  )) |>
  mutate(
    commodity = str_remove(commodity, " Coal")
  ) |>
  select(year, commodity, production_value) |>
  group_by(year, commodity) |>
  summarise(n = sum(production_value)) |>
  ungroup() |>
  filter(year >= 1900)

exceeds100 <- plot_data |> 
  group_by(year) |> 
  summarise(n = sum(n)) |> 
  filter(n > 100) |> 
  pull(year) |> 
  min()

orders <- plot_data |> 
  filter(year == 2022) |> 
  arrange(-n) |> 
  pull(commodity)

plot_data <- plot_data |> 
  mutate(commodity = factor(commodity, levels = orders)) |> 
  arrange(year, commodity)


# Define colours and fonts-------------------------------------------------

bg_col <- "#FFFFFA"
text_col <- "#0D5C63"
col_palette <- rcartocolor::carto_pal(n = 7, "Vivid")[1:6]
names(col_palette) <- unique(plot_data$commodity)
highlight_col <- col_palette[3]

body_font <- "roboto"
title_font <- "robotoslab"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-05-21", "recording"),
  device = "png",
  width = 8,
  height = 6,
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
title <- "<span style='font-size: 56pt;'>**Coal production since 1900**</span><br>"
st <- "Carbon Majors is a database of historical production data from 122 of
the world’s largest oil, gas, coal, and cement producers. This data is used to
quantify the direct operational emissions and emissions from the combustion of
marketed products that can be attributed to these entities."
txt <- paste0(
  title, st
)
cap <- paste0(
  "**Data**: Carbon Majors<br>**Graphic**:", social
)

legend_txt <- glue::glue(
  "**Coal types**<br>Total coal production includes production of 
  <span style='color:{col_palette[[1]]}'>{names(col_palette[1])}</span>,
  <span style='color:{col_palette[[2]]}'>{names(col_palette[2])}</span>,
  <span style='color:{col_palette[[3]]}'>{names(col_palette[3])}</span>,
  <span style='color:{col_palette[[4]]}'>{names(col_palette[4])}</span>,
  <span style='color:{col_palette[[5]]}'>{names(col_palette[5])}</span>,
  and <span style='color:{col_palette[[6]]}'>{names(col_palette[6])}</span> coal. 
  Bituminous accounts for around half."
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = year, y = n)
) +
  # axis labels
  geom_segment(
    data = data.frame(year = seq(1900, 2020, 20)),
    mapping = aes(x = year, xend = year, y = 0, yend = -4500),
    linetype = "dashed",
    alpha = 0.4,
    colour = text_col
  ) +
  geom_text(
    data = data.frame(year = seq(1900, 2020, 20)),
    mapping = aes(x = year, y = -4700, label = year),
    colour = text_col,
    family = body_font,
    size = 8
  ) +
  # story 1
  annotate("segment",
    x = exceeds100, xend = exceeds100,
    y = 0, yend = 2000, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = exceeds100, y = 2000,
      label = glue(
        "**{exceeds100}**<br>Total coal production first exceeds 100 million tonnes per year."
        )
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 8,
    lineheight = 0.4,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # story 2
  annotate("segment",
           x = 1990, xend = 1990,
           y = 0, yend = 5000, color = text_col
  ) +
  geom_textbox(
    data = data.frame(
      x = 1990, y = 5000,
      label = legend_txt
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    vjust = 1,
    valign = 1,
    size = 8,
    lineheight = 0.4,
    hjust = 0,
    halign = 0,
    box.colour = "transparent",
    fill = "transparent"
  ) +
  # stream plot
  geom_stream(aes(fill = commodity),
    bw = 0.3, extra_span = 0.003, sorting = "onset"
  ) +
  labs(
    tag = txt,
    caption = cap
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  scale_x_continuous(limits = c(1896, 2025)) +
  scale_y_continuous(limits = c(-4800, 6000)) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 26, base_family = body_font) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.005, 0.98),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      valign = 1,
      vjust = 1,
      margin = margin(l = 10, b = 15, t = 5),
      lineheight = 0.5,
      maxwidth = 0.8,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(l = 10, b = 0, t = 15),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-05-21", paste0("20240521", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
