# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(countrycode)
library(emojifont)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-09-08")
cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#f5eee7"
text_col <- "#341b06"


# Data wrangling ----------------------------------------------------------

plot_data <- cappuccino_index |>
  mutate(value = 60 / index) |>
  select(country, value) |>
  mutate(continent = countrycode(country, "country.name", "continent")) |>
  filter(continent == "Europe") |>
  select(-continent) |>
  arrange(value) |>
  mutate(value = round(value)) |> 
  mutate(country = factor(country, levels = country)) |>
  uncount(value) |>
  group_by(country) |>
  mutate(x = row_number()) |>
  ungroup() |>
  mutate(text = fontawesome("fa-coffee"))


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- glue("<span style='font-family:{title_font}; font-size: 20pt;'>**Italy is one cup ahead**</span><br>How many small cappuccinos can a barista buy with one hour of pay in different European countries?")
cap <- source_caption(
  source = "The Cappuccino Index. James Hoffmann. YouTube.", graphic = social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_text(
    data = plot_data,
    mapping = aes(x = x, y = country, label = text),
    family = "fontawesome-webfont",
    colour = text_col,
    size = 4.5
  ) +
  scale_x_continuous(
    limits = c(0.5, max(plot_data$x) + 0.5),
    expand = expansion(0, 0)
  ) +
  labs(
    x = NULL, y = NULL,
    tag = title,
    caption = cap
  ) +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.tag.position = c(0.98, 0.35),
    plot.margin = margin(5, 10, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 5),
      family = body_font,
      maxwidth = 0.4
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    panel.grid = element_blank(),
    axis.text.y = element_text(
      hjust = 1,
      margin = margin(r = 3),
      family = body_font,
      colour = text_col
    )
  ) +
  canvas(
    width = 4, height = 8,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = file.path("2026", "2026-09-08", paste0("20260908", ".png"))
)
