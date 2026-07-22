# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-08-04")
basotho_wool <- tuesdata$basotho_wool


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#FFFFFF"
text_col <- "#001489"
highlight_col <- "#a1d64b"

img <- paste0(here::here(), "/2026/2026-08-04/wool.png")


# Data wrangling ----------------------------------------------------------

plot_data <- basotho_wool |>
  filter(cmd_code == 5101, ref_year == 2023) |>
  group_by(reporter_desc) |>
  select(reporter_desc, net_wgt) |>
  drop_na() |>
  summarise(
    weight_kg = sum(net_wgt),
    weight_tonnes = weight_kg / 1000
  ) |>
  arrange(weight_tonnes) |>
  mutate(reporter_desc = factor(reporter_desc, levels = reporter_desc))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = "#009A44",
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
title <- "Who imports the most Basotho wool?"
st <- glue("Lesotho, whose people are referred to as *Basotho*, is internationally known for high-quality mohair from Angora goats, prized in luxury fashion and textile industries for its softness and durability. Wool and mohair have become one of the country’s most valuable agricultural exports, and in 2023 countries imported over 8,500 tonnes of wool.")
cap <- paste0("**Image**: pngtree.com | ", source_caption(source = "UN Comtrade Database", graphic = social, sep = " | "))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_segment(
    data = plot_data,
    mapping = aes(x = 0, xend = weight_tonnes, y = reporter_desc),
    linewidth = 1.4,
    colour = highlight_col
  ) +
  geom_image(
    data = plot_data,
    mapping = aes(
      x = weight_tonnes + 440,
      y = (1:4) + 0.32,
      image = img
    ),
    size = 0.3
  ) +
  geom_richtext(
    data = plot_data,
    mapping = aes(
      x = weight_tonnes + 1600,
      y = (1:4) + 0.29,
      label = glue("**{reporter_desc}**<br>{formatC(round(weight_tonnes), big.mark = ',')} tonnes")
    ),
    hjust = 0,
    colour = text_col,
    label.r = unit(0, "lines"),
    label.colour = "transparent",
    fill = alpha(bg_col, 0.5)
  ) +
  scale_x_continuous(
    labels = scales::label_comma(),
    expand = expansion(0, 0)
  ) +
  labs(x = NULL, y = NULL,
       title = title,
       subtitle = st,
       caption = cap) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(10, 80, 10, 35),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5, l = -25),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      width = 1.2,
      margin = margin(b = 30, t = 5, l = -25),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = -20, l = -25),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.9)
    ),
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  canvas(
    width = 6, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-08-04", paste0("20260804", ".png"))
)
